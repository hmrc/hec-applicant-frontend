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
import cats.implicits._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.views.YesNoOption
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
  cannotDoTaxCheckPage: html.CannotDoTaxCheck,
  ctutrNotMatchedPage: html.CtutrNotMatched,
  noAccountingPeriodFoundPage: html.NoAccountingPeriodFound,
  mcc: MessagesControllerComponents
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private def getPage(form: Form[YesNoAnswer], companyName: CompanyHouseName)(implicit
    request: RequestWithSessionData[_]
  ): Result = {
    val back = journeyService.previous(routes.CompanyDetailsController.confirmCompanyDetails())
    Ok(
      confirmCompanyNamePage(
        form,
        back,
        companyName.name,
        CompanyDetailsController.companyNameConfirmedOptions
      )
    )
  }

  val confirmCompanyDetails: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    ensureCompanyRetrievedData(request.sessionData) { (_, companyHouseName) =>
      val companyDetailsConfirmed =
        request.sessionData.userAnswers.fold(_.companyDetailsConfirmed, _.companyDetailsConfirmed)
      val form = {
        val emptyForm = CompanyDetailsController.confirmCompanyNameForm(YesNoAnswer.values)
        companyDetailsConfirmed.fold(emptyForm)(emptyForm.fill)
      }
      Future.successful(getPage(form, companyHouseName))
    }
  }

  val confirmCompanyDetailsSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      def internalServerError(errorMessage: String)(e: Error) = {
        logger.warn(errorMessage, e)
        InternalServerError
      }

      def callUpdateAndNext(updatedSession: HECSession) =
        journeyService
          .updateAndNext(
            routes.CompanyDetailsController.confirmCompanyDetails(),
            updatedSession
          )

      def fetchCTStatus(
        desCtutrOpt: Option[CTUTR],
        companyData: CompanyRetrievedData
      ): EitherT[Future, Error, Option[CTStatusResponse]] =
        companyData.ctutr flatTraverse [EitherT[Future, Error, *], CTStatusResponse] { ctutr =>
          desCtutrOpt match {
            case Some(desCtutr) if desCtutr.value === ctutr.value =>
              val (start, end) = CompanyDetailsController.calculateLookbackPeriod(timeProvider.currentDate)
              taxCheckService.getCTStatus(desCtutr, start, end)
            case _                                                =>
              EitherT.fromEither[Future](Right[Error, Option[CTStatusResponse]](None))
          }
        }

      def fetchDataAndProceed(
        companyDetailsConfirmed: YesNoAnswer,
        companyData: CompanyRetrievedData,
        crn: CRN
      ): Future[Result] = {
        val result = for {
          desCtutr            <- taxCheckService.getCtutr(crn)
          ctStatusOpt         <- fetchCTStatus(desCtutr, companyData)
          updatedRetrievedData = companyData.copy(desCtutr = desCtutr, ctStatus = ctStatusOpt)
          updatedUserAnswers   = request.sessionData.userAnswers
                                   .unset(_.companyDetailsConfirmed)
                                   .copy(companyDetailsConfirmed = Some(companyDetailsConfirmed))
          updatedSession       = request.sessionData.copy(
                                   retrievedUserData = updatedRetrievedData,
                                   userAnswers = updatedUserAnswers
                                 )
          call                <- callUpdateAndNext(updatedSession)
        } yield call

        result.fold(
          internalServerError("Could not update session and proceed"),
          Redirect
        )
      }

      def handleValidAnswer(companyData: CompanyRetrievedData, crn: CRN)(
        companyDetailsConfirmed: YesNoAnswer
      ): Future[Result] =
        companyDetailsConfirmed match {
          case YesNoAnswer.Yes =>
            fetchDataAndProceed(companyDetailsConfirmed, companyData, crn)

          case YesNoAnswer.No =>
            // wipe CRN answer prior to navigating to next page
            val answersWithoutCrn = request.sessionData.userAnswers.unset(_.crn)
            callUpdateAndNext(request.sessionData.copy(userAnswers = answersWithoutCrn)).fold(
              internalServerError("Could not update session and proceed"),
              Redirect
            )
        }

      def getFuturePage(companyHouseName: CompanyHouseName)(form: Form[YesNoAnswer])(implicit
        request: RequestWithSessionData[_]
      ) =
        Future.successful(getPage(form, companyHouseName))

      ensureCompanyRetrievedData(request.sessionData) { (companyData, companyHouseName) =>
        ensureCorrectUserAnswersState(request.sessionData) { crn =>
          CompanyDetailsController
            .confirmCompanyNameForm(YesNoAnswer.values)
            .bindFromRequest()
            .fold(getFuturePage(companyHouseName), handleValidAnswer(companyData, crn))
        }
      }
    }

  val noAccountingPeriod: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.noAccountingPeriod())
    Ok(noAccountingPeriodFoundPage(back))
  }

  val chargeableForCorporationTax: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

  val ctutrNotMatched: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.ctutrNotMatched())
    Ok(ctutrNotMatchedPage(back))
  }

  val enterCtutr: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

  val cannotDoTaxCheck: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.cannotDoTaxCheck())
    Ok(cannotDoTaxCheckPage(back))
  }

  private def ensureCompanyRetrievedData(
    session: HECSession
  )(f: (CompanyRetrievedData, CompanyHouseName) => Future[Result]): Future[Result] =
    session.retrievedUserData match {
      case CompanyRetrievedData(_, _, _, None, _, _, _)                  =>
        logger.warn("Missing company name")
        InternalServerError
      case _: IndividualRetrievedData                                    =>
        logger.warn("Individual applicant shouldn't call company confirm page")
        InternalServerError
      case c @ CompanyRetrievedData(_, _, _, Some(companyName), _, _, _) => f(c, companyName)
    }

  private def ensureCorrectUserAnswersState(
    session: HECSession
  )(f: CRN => Future[Result]): Future[Result] =
    session.userAnswers match {
      case UserAnswers.IncompleteUserAnswers(_, _, _, _, _, _, Some(crn), _) => f(crn)
      case UserAnswers.CompleteUserAnswers(_, _, _, _, _, _, Some(crn), _)   => f(crn)
      case _                                                                 =>
        logger.warn("CRN is not populated in user answers")
        InternalServerError
    }
}

object CompanyDetailsController {
  val companyNameConfirmedOptions: List[YesNoOption] = YesNoAnswer.values.map(YesNoOption.yesNoOption)

  def confirmCompanyNameForm(options: List[YesNoAnswer]): Form[YesNoAnswer] =
    Form(
      mapping(
        "confirmCompanyName" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

  /**
    * Calculate the lookback period based on today's date.
    * The lookback period is the the most recent accounting period of the company to have ended 12 months or more
    * before the day on which the tax check is initiated. (These are the dates used when retrieving the Corporation tax
    * records for the Applicant's company using the Get Company Accounting Periods API.)
    */
  def calculateLookbackPeriod(today: LocalDate): (LocalDate, LocalDate) =
    today.minusYears(2).plusDays(1) -> today.minusYears(1)
}
