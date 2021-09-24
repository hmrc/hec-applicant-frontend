/*
 * Copyright 2021 HM Revenue & Customs
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
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.CRNController.crnForm
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.services.{CompanyDetailsService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

class CRNController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  companyDetailsService: CompanyDetailsService,
  crnPage: html.CompanyRegistrationNumber,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext, appConfig: AppConfig)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val companyRegistrationNumber: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val crn  = request.sessionData.userAnswers.fold(_.crn, _.crn)
    val back = journeyService.previous(routes.CRNController.companyRegistrationNumber())
    val form = crn.fold(crnForm)(crnForm.fill)
    Ok(crnPage(form, back))
  }

  val companyRegistrationNumberSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      crnForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              crnPage(
                formWithErrors,
                journeyService.previous(routes.CRNController.companyRegistrationNumber())
              )
            ),
          handleValidCrn
        )

    }

  private def checkCompanyName(crn: CRN)(implicit request: RequestWithSessionData[_]): EitherT[Future, Error, Call] =
    for {
      companyHouseDetailsOpt <- companyDetailsService.findCompany(crn)
      updatedAnswers          =
        request.sessionData.userAnswers
          .unset(_.crn)
          .unset(_.companyName)
          .copy(crn = Some(crn), companyName = companyHouseDetailsOpt.map(_.companyName))
      updatedSession          = request.sessionData.copy(userAnswers = updatedAnswers)
      next                   <- journeyService
                                  .updateAndNext(routes.CRNController.companyRegistrationNumber(), updatedSession)
    } yield next

  private def handleValidCrn(crn: CRN)(implicit request: RequestWithSessionData[_]): Future[Result] =
    checkCompanyName(crn)
      .fold(
        { e =>
          logger.warn(" Couldn't get company Name from the given CRN", e)
          InternalServerError
        },
        Redirect
      )

}

object CRNController {

  //This regex checks first two characters as alphanumeric but the next 5/6 chars should be number
  private val crnRegex = "^[A-Z0-9]{2}[0-9]{5,6}"

  //Checking CRN constraint based on rules on following priority
  //Should have only alphanumeric characters
  //Should be in correct format - first two chars alphanumeric and rest 5/6 chars as number
  // and Should have only either 7 or 8 characters
  private val crnConstraint: Constraint[CRN] =
    Constraint(code =>
      if (!code.value.forall(_.isLetterOrDigit)) Invalid("error.nonAlphanumericChars")
      else if (code.value.matches(crnRegex)) Valid
      else Invalid("error.crnInvalid")
    )

  val crnForm: Form[CRN] =
    Form(
      mapping(
        "crn" -> nonEmptyText
          .transform[CRN](
            s => CRN(s.removeWhitespace.toUpperCase(Locale.UK)),
            _.value
          )
          .verifying(crnConstraint)
      )(identity)(Some(_))
    )
}
