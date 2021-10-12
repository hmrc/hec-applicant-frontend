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
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.CompanyLoginData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.views.YesNoOption
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider, TimeUtils}
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
  chargeableForCTPage: html.ChargeableForCT,
  ctIncomeStatementPage: html.CTIncomeStatement,
  mcc: MessagesControllerComponents
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private def getConfirmCompanyDetailsPage(form: Form[YesNoAnswer], companyName: CompanyHouseName)(implicit
    request: RequestWithSessionData[_]
  ): Result = {
    val back = journeyService.previous(routes.CompanyDetailsController.confirmCompanyDetails())
    Ok(
      confirmCompanyNamePage(
        form,
        back,
        companyName.name,
        YesNoOption.yesNoOptions
      )
    )
  }

  val confirmCompanyDetails: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData mapAsCompany { companySession =>
      ensureCompanyDataHasCompanyName(companySession) { companyHouseName =>
        val companyDetailsConfirmed =
          request.sessionData.userAnswers.fold(_.companyDetailsConfirmed, _.companyDetailsConfirmed)
        val form = {
          val emptyForm = CompanyDetailsController.confirmCompanyNameForm(YesNoAnswer.values)
          companyDetailsConfirmed.fold(emptyForm)(emptyForm.fill)
        }
        Future.successful(getConfirmCompanyDetailsPage(form, companyHouseName))
      }
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
        companyLoginData: CompanyLoginData
      ): EitherT[Future, Error, Option[CTStatusResponse]] =
        companyLoginData.ctutr flatTraverse [EitherT[Future, Error, *], CTStatusResponse] { ctutr =>
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
        session: CompanyHECSession,
        crn: CRN
      ): Future[Result] = {
        val result = for {
          desCtutr            <- taxCheckService.getCtutr(crn)
          ctStatusOpt         <- fetchCTStatus(desCtutr, session.loginData)
          updatedRetrievedData = session.retrievedJourneyData.copy(desCtutr = desCtutr, ctStatus = ctStatusOpt)
          updatedUserAnswers   = session.userAnswers
                                   .unset(_.companyDetailsConfirmed)
                                   .copy(companyDetailsConfirmed = Some(companyDetailsConfirmed))
          updatedSession       = session.copy(retrievedJourneyData = updatedRetrievedData, userAnswers = updatedUserAnswers)
          call                <- callUpdateAndNext(updatedSession)
        } yield call

        result.fold(
          internalServerError("Could not update session and proceed"),
          Redirect
        )
      }

      def handleValidAnswer(session: CompanyHECSession, crn: CRN)(
        companyDetailsConfirmed: YesNoAnswer
      ): Future[Result] =
        companyDetailsConfirmed match {
          case YesNoAnswer.Yes =>
            fetchDataAndProceed(companyDetailsConfirmed, session, crn)

          case YesNoAnswer.No =>
            // wipe CRN answer prior to navigating to next page
            val answersWithoutCrn = request.sessionData.userAnswers
              .unset(_.crn)
              .unset(_.companyDetailsConfirmed)
              .copy(companyDetailsConfirmed = Some(companyDetailsConfirmed))
            callUpdateAndNext(session.copy(userAnswers = answersWithoutCrn)).fold(
              internalServerError("Could not update session and proceed"),
              Redirect
            )
        }

      def getFuturePage(companyHouseName: CompanyHouseName)(form: Form[YesNoAnswer])(implicit
        request: RequestWithSessionData[_]
      ) =
        Future.successful(getConfirmCompanyDetailsPage(form, companyHouseName))

      request.sessionData mapAsCompany { companySession =>
        ensureCompanyDataHasCompanyName(companySession) { companyHouseName =>
          ensureUserAnswersHasCRN(request.sessionData) { crn =>
            CompanyDetailsController
              .confirmCompanyNameForm(YesNoAnswer.values)
              .bindFromRequest()
              .fold(getFuturePage(companyHouseName), handleValidAnswer(companySession, crn))
          }
        }
      }
    }

  val noAccountingPeriod: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.noAccountingPeriod())
    Ok(noAccountingPeriodFoundPage(back))
  }

  val chargeableForCorporationTax: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData mapAsCompany { companySession =>
        ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
          val chargeableForCT = request.sessionData.userAnswers.fold(_.chargeableForCT, _.chargeableForCT)
          val form = {
            val emptyForm = CompanyDetailsController.chargeableForCTForm(YesNoAnswer.values)
            chargeableForCT.fold(emptyForm)(emptyForm.fill)
          }
          Ok(
            chargeableForCTPage(
              form = form,
              back = journeyService.previous(routes.CompanyDetailsController.chargeableForCorporationTax()),
              date = TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate),
              options = YesNoOption.yesNoOptions
            )
          )
        }
      }
    }

  val chargeableForCorporationTaxSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      def handleValidAnswer(chargeableForCT: YesNoAnswer) = {
        val updatedAnswers =
          request.sessionData.userAnswers.unset(_.chargeableForCT).copy(chargeableForCT = Some(chargeableForCT))

        journeyService
          .updateAndNext(
            routes.CompanyDetailsController.chargeableForCorporationTax(),
            request.sessionData.mapAsCompany(_.copy(userAnswers = updatedAnswers))
          )
          .fold(
            { e =>
              logger.warn("Could not update session and proceed", e)
              InternalServerError
            },
            Redirect
          )
      }

      request.sessionData mapAsCompany { companySession =>
        ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
          CompanyDetailsController
            .chargeableForCTForm(YesNoAnswer.values)
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Ok(
                  chargeableForCTPage(
                    form = formWithErrors,
                    back = journeyService.previous(routes.CompanyDetailsController.chargeableForCorporationTax()),
                    date = TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate),
                    options = YesNoOption.yesNoOptions
                  )
                ),
              handleValidAnswer
            )
        }
      }
    }

  val ctutrNotMatched: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.ctutrNotMatched())
    Ok(ctutrNotMatchedPage(back))
  }

  val enterCtutr: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

  val ctIncomeStatement: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData mapAsCompany { companySession =>
      ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
        val back             = journeyService.previous(routes.CompanyDetailsController.ctIncomeStatement())
        val ctIncomeDeclared = request.sessionData.userAnswers.fold(_.ctIncomeDeclared, _.ctIncomeDeclared)
        val form = {
          val emptyForm = CompanyDetailsController.ctIncomeStatementForm(YesNoAnswer.values)
          ctIncomeDeclared.fold(emptyForm)(emptyForm.fill)
        }
        Ok(
          ctIncomeStatementPage(
            form,
            back,
            YesNoOption.yesNoOptions,
            TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate)
          )
        )
      }
    }
  }

  val ctIncomeStatementSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.mapAsCompany { companySession =>
      ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
        def handleValidAnswer(incomeDeclared: YesNoAnswer): Future[Result] = {
          val updatedAnswers =
            companySession.userAnswers.unset(_.ctIncomeDeclared).copy(ctIncomeDeclared = Some(incomeDeclared))

          journeyService
            .updateAndNext(
              routes.CompanyDetailsController.ctIncomeStatement(),
              companySession.copy(userAnswers = updatedAnswers)
            )
            .fold(
              { e =>
                logger.warn("Could not update session and proceed", e)
                InternalServerError
              },
              Redirect
            )
        }

        CompanyDetailsController
          .ctIncomeStatementForm(YesNoAnswer.values)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                ctIncomeStatementPage(
                  formWithErrors,
                  journeyService.previous(routes.CompanyDetailsController.ctIncomeStatement()),
                  YesNoOption.yesNoOptions,
                  TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate)
                )
              ),
            handleValidAnswer
          )
      }
    }
  }

  val cannotDoTaxCheck: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.cannotDoTaxCheck())
    Ok(cannotDoTaxCheckPage(back))
  }

  private def ensureCompanyDataHasCompanyName(
    companySession: CompanyHECSession
  )(f: CompanyHouseName => Future[Result]): Future[Result] =
    companySession.retrievedJourneyData.companyName match {
      case Some(companyName) => f(companyName)
      case None              =>
        logger.warn("Missing company name")
        InternalServerError
    }

  private def ensureCompanyDataHasCTStatusAccountingPeriod(
    companySession: CompanyHECSession
  )(f: CTAccountingPeriod => Future[Result]): Future[Result] =
    companySession.retrievedJourneyData.ctStatus match {
      case Some(CTStatusResponse(_, _, _, Some(latestAccountingPeriod))) => f(latestAccountingPeriod)
      case Some(_)                                                       =>
        logger.warn("Missing CT status latest accounting period")
        InternalServerError
      case None                                                          =>
        logger.warn("Missing CT status")
        InternalServerError
    }

  private def ensureUserAnswersHasCRN(
    session: HECSession
  )(f: CRN => Future[Result]): Future[Result] =
    session.userAnswers match {
      case UserAnswers.IncompleteUserAnswers(_, _, _, _, _, _, Some(crn), _, _, _) => f(crn)
      case UserAnswers.CompleteUserAnswers(_, _, _, _, _, _, Some(crn), _, _, _)   => f(crn)
      case _                                                                       =>
        logger.warn("CRN is not populated in user answers")
        InternalServerError
    }
}

object CompanyDetailsController {
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

  def chargeableForCTForm(options: List[YesNoAnswer]): Form[YesNoAnswer] =
    Form(
      mapping(
        "chargeableForCT" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

  def ctIncomeStatementForm(options: List[YesNoAnswer]): Form[YesNoAnswer] =
    Form(
      mapping(
        "ctIncomeDeclared" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )
}
