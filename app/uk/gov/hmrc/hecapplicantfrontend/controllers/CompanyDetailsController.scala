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
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.CompanyRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CTUTR
import cats.implicits._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.models.views.CompanyNameConfirmedOption
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

class CompanyDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  taxCheckService: TaxCheckService,
  timeProvider: TimeProvider,
  confirmCompanyNamePage: html.ConfirmCompanyName,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private def getPage(form: Form[CompanyNameConfirmed])(implicit request: RequestWithSessionData[_]): Result = {
    val back = journeyService.previous(routes.CompanyDetailsController.confirmCompanyDetails())
    request.sessionData.retrievedUserData match {
      case RetrievedApplicantData.CompanyRetrievedData(_, _, _, Some(companyHouseName), _, _, _) =>
        Ok(
          confirmCompanyNamePage(
            form,
            back,
            companyHouseName.name,
            CompanyDetailsController.companyNameConfirmedOptions
          )
        )
      case RetrievedApplicantData.CompanyRetrievedData(_, _, _, None, _, _, _)                   =>
        logger.warn("Missing company name")
        InternalServerError
      case _: RetrievedApplicantData.IndividualRetrievedData                                     =>
        logger.warn("Confirm company name called for individual applicant")
        InternalServerError
    }
  }

  val confirmCompanyDetails: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val companyNameConfirmed = request.sessionData.userAnswers.fold(_.companyNameConfirmed, _.companyNameConfirmed)
    val form = {
      val emptyForm = CompanyDetailsController.confirmCompanyNameForm(CompanyNameConfirmed.values)
      companyNameConfirmed.fold(emptyForm)(emptyForm.fill)
    }
    getPage(form)
  }

  val confirmCompanyDetailsSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      def internalServerError(errorMessage: String) = {
        logger.warn(errorMessage)
        InternalServerError
      }

      def callUpdateAndNext(updatedSession: HECSession) =
        journeyService
          .updateAndNext(
            routes.CompanyDetailsController.confirmCompanyDetails(),
            updatedSession
          )
          .fold(
            { e =>
              logger.warn("Could not update session and proceed", e)
              InternalServerError
            },
            Redirect
          )

      def fetchCTStatus(
        desCtutr: CTUTR,
        companyData: CompanyRetrievedData
      ): EitherT[Future, Error, Option[CTStatusResponse]] =
        companyData.ctutr traverse [EitherT[Future, Error, *], CTStatusResponse] { ctutr =>
          if (desCtutr.value === ctutr.value) {
            val (start, end) = CompanyDetailsController.calculateLookbackPeriod(timeProvider.currentDate)
            taxCheckService.getCTStatus(desCtutr, start, end)
          } else {
            EitherT
              .fromEither[Future](
                Left[Error, CTStatusResponse](
                  Error("CTUTR from DES does not match the retrieved value")
                )
              )
          }
        }

      def fetchDataAndProceed(crn: CRN, updatedUserAnswers: UserAnswers): Future[Result] = {
        val updatedSession = for {
          desCtutr    <- taxCheckService.getCtutr(crn)
          companyData <-
            EitherT.fromEither[Future](request.sessionData.retrievedUserData match {
              case companyData: RetrievedApplicantData.CompanyRetrievedData =>
                Right(companyData)
              case _: RetrievedApplicantData.IndividualRetrievedData        =>
                Left(Error("Individual applicant data found in company journey"))
            })
          ctStatusOpt <- fetchCTStatus(desCtutr, companyData)
        } yield {
          val updatedRetrievedData = companyData.copy(desCtutr = Some(desCtutr), ctStatus = ctStatusOpt)
          request.sessionData.copy(
            retrievedUserData = updatedRetrievedData,
            userAnswers = updatedUserAnswers
          )
        }

        updatedSession.foldF(
          { e =>
            logger.warn("Could not update session and proceed", e)
            Future.successful(InternalServerError)
          },
          callUpdateAndNext
        )
      }

      def handleValidAnswer(companyNameConfirmed: CompanyNameConfirmed): Future[Result] = {
        val updatedUserAnswers = request.sessionData.userAnswers
          .unset(_.companyNameConfirmed)
          .copy(companyNameConfirmed = Some(companyNameConfirmed))

        companyNameConfirmed match {
          case CompanyNameConfirmed.Yes =>
            val crnOpt = request.sessionData.userAnswers.fold(_.crn, _.crn)
            crnOpt match {
              case Some(crn) => fetchDataAndProceed(crn, updatedUserAnswers)
              case None      => internalServerError("No CRN found in session")
            }

          case CompanyNameConfirmed.No =>
            callUpdateAndNext(request.sessionData.copy(userAnswers = updatedUserAnswers))
        }
      }

      def getFuturePage(form: Form[CompanyNameConfirmed])(implicit request: RequestWithSessionData[_]) =
        Future.successful(getPage(form))

      CompanyDetailsController
        .confirmCompanyNameForm(CompanyNameConfirmed.values)
        .bindFromRequest()
        .fold(getFuturePage, handleValidAnswer)
    }

  // TODO implement this page - https://hec-applicant-tax-check.herokuapp.com/v6/no-account-period-found
  val noAccountingPeriod: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

  val chargeableForCorporationTax: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

  // TODO implement this page - https://hec-applicant-tax-check.herokuapp.com/v6/exit-pages/ct-utr-cannot-match
  val ctutrNotMatched: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

  val enterCtutr: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }
}

object CompanyDetailsController {
  val companyNameConfirmedOptions: List[CompanyNameConfirmedOption] =
    CompanyNameConfirmed.values.map(CompanyNameConfirmedOption.companyNameConfirmedToOption)

  def confirmCompanyNameForm(options: List[CompanyNameConfirmed]): Form[CompanyNameConfirmed] =
    Form(
      mapping(
        "confirmCompanyName" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

  // TODO ensure you've understood this correctly
  // TODO add docstring
  def calculateLookbackPeriod(today: LocalDate): (LocalDate, LocalDate) = {
    val minus1Year  = today.minusYears(1)
    val minus2Years = today.minusYears(2)
    (minus2Years, minus1Year)
  }
}
