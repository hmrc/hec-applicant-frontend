/*
 * Copyright 2023 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.option._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.CRNController.crnForm
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.services.{CompanyDetailsService, CtutrAttemptsService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.ControllerUtils.noXssChars
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

class CRNController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  companyDetailsService: CompanyDetailsService,
  ctutrAttemptsService: CtutrAttemptsService,
  crnPage: html.CompanyRegistrationNumber,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext, appConfig: AppConfig)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val companyRegistrationNumber: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.mapAsCompany { implicit companySession =>
      val crn  = companySession.userAnswers.fold(_.crn, _.crn.some)
      val back = journeyService.previous(routes.CRNController.companyRegistrationNumber)
      val form = crn.fold(crnForm())(crnForm().fill)
      Ok(crnPage(form, back))
    }
  }

  val companyRegistrationNumberSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.mapAsCompany { implicit companySession =>
        def ok(form: Form[CRN]) =
          Ok(
            crnPage(
              form,
              journeyService.previous(routes.CRNController.companyRegistrationNumber)
            )
          )

        def updateAndNext(updatedSession: HECSession): EitherT[Future, Error, Either[Form[CRN], Call]] =
          journeyService
            .updateAndNext(routes.CRNController.companyRegistrationNumber, updatedSession)
            .map(Right[Form[CRN], Call])

        def updateAnswers(crn: CRN) =
          companySession.userAnswers
            .unset(_.crn)
            .unset(_.companyDetailsConfirmed)
            .unset(_.chargeableForCT)
            .unset(_.ctIncomeDeclared)
            .unset(_.recentlyStartedTrading)
            .unset(_.ctutr)
            .copy(crn = Some(crn))

        def fetchCompanyNameAndProceed(crn: CRN): EitherT[Future, Error, Either[Form[CRN], Call]] = for {
          companyHouseDetailsOpt <- companyDetailsService.findCompany(crn)
          updatedRetrievedData    =
            companySession.retrievedJourneyData.copy(
              companyName = companyHouseDetailsOpt.map(_.companyName)
            )
          updatedSession          = companySession.copy(
                                      retrievedJourneyData = updatedRetrievedData,
                                      userAnswers = updateAnswers(crn),
                                      crnBlocked = false
                                    )
          formErrorOrNext        <- companyHouseDetailsOpt map { _ =>
                                      updateAndNext(updatedSession)
                                    } getOrElse {
                                      val constraint: Constraint[CRN] =
                                        Constraint(_ => Invalid("error.notFoundInCompaniesHouse"))
                                      EitherT.pure[Future, Error](Left(crnForm(constraint).bindFromRequest()))
                                    }
        } yield formErrorOrNext

        def handleValidCrn(crn: CRN): Future[Result] = {

          def crnsMatch: Boolean = {
            val sessionCrn = companySession.userAnswers.fold(_.crn, _.crn.some)
            sessionCrn.contains(crn)
          }

          val result = for {
            ctutrAttemptsOpt <- ctutrAttemptsService.get(crn, companySession.loginData.ggCredId)
            isBlocked         = ctutrAttemptsOpt.exists(_.isBlocked)
            eitherResult     <- if (isBlocked) {
                                  val updatedSession =
                                    companySession.copy(userAnswers = updateAnswers(crn), crnBlocked = true)
                                  updateAndNext(updatedSession)
                                } else if (crnsMatch) {
                                  updateAndNext(companySession.copy(crnBlocked = false))
                                } else {
                                  fetchCompanyNameAndProceed(crn)
                                }
          } yield eitherResult

          result.fold(
            _.doThrow("Could not update session and proceed"),
            _.fold(ok, Redirect)
          )
        }

        crnForm()
          .bindFromRequest()
          .fold(ok, handleValidCrn)
      }
    }

}

object CRNController {

  // This regex checks first two characters as alphanumeric but the next 5/6 chars should be number
  private val crnRegex = "^[A-Z0-9]{2}[0-9]{5,6}"

  // Checking CRN constraint based on rules on following priority
  // Should have only alphanumeric characters
  // Should be in correct format - first two chars alphanumeric and rest 5/6 chars as number
  // and Should have only either 7 or 8 characters
  val crnConstraint: Constraint[CRN] =
    Constraint(crn =>
      if (!crn.value.forall(_.isLetterOrDigit)) Invalid("error.nonAlphanumericChars")
      else if (crn.value.matches(crnRegex)) Valid
      else Invalid("error.crnInvalid")
    )

  def crnForm(constraint: Constraint[CRN] = crnConstraint): Form[CRN] =
    Form(
      mapping(
        "crn" -> nonEmptyText
          .verifying(noXssChars("error.nonAlphanumericChars"))
          .transform[CRN](
            s => CRN(s.removeWhitespace.toUpperCase(Locale.UK)),
            _.value
          )
          .verifying(constraint)
      )(identity)(Some(_))
    )
}
