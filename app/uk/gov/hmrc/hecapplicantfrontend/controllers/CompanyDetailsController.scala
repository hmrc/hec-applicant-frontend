/*
 * Copyright 2022 HM Revenue & Customs
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
import play.api.data.Forms.{mapping, nonEmptyText, of}
import play.api.data.validation.{Constraint, Invalid, Valid}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.CompanyDetailsController.{enterCtutrForm, enterCtutrFormKey}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchFailure.{EnrolmentCTUTRCompanyMatchFailure, EnterCTUTRCompanyMatchFailure}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchSuccess.{EnrolmentCTUTRCompanyMatchSuccess, EnterCTUTRCompanyMatchSuccess}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckExit
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.CompanyLoginData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.{CTAccountingPeriod, CTStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.views.YesNoOption
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, CtutrAttemptsService, JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider, TimeUtils}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.{LocalDate, ZoneId}
import scala.concurrent.{ExecutionContext, Future}

class CompanyDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  taxCheckService: TaxCheckService,
  auditService: AuditService,
  timeProvider: TimeProvider,
  confirmCompanyNamePage: html.ConfirmCompanyName,
  cannotDoTaxCheckPage: html.CannotDoTaxCheck,
  ctutrNotMatchedPage: html.CtutrNotMatched,
  recentlyStartedTradingPage: html.RecentlyStartedTrading,
  chargeableForCTPage: html.ChargeableForCT,
  ctIncomeStatementPage: html.CTIncomeStatement,
  enterCtutrPage: html.EnterCtutr,
  dontHaveCtutrPage: html.DontHaveCtutr,
  tooManyCTUTRAttemptsPage: html.TooManyCtutrAttempts,
  sessionStore: SessionStore,
  ctutrAttemptsService: CtutrAttemptsService,
  mcc: MessagesControllerComponents
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private def getConfirmCompanyDetailsPage(form: Form[YesNoAnswer], companyName: CompanyHouseName)(implicit
    request: RequestWithSessionData[_]
  ): Result = {
    val back = journeyService.previous(routes.CompanyDetailsController.confirmCompanyDetails)
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
          companySession.userAnswers.fold(_.companyDetailsConfirmed, _.companyDetailsConfirmed.some)
        val form = {
          val emptyForm = CompanyDetailsController.yesNoForm("confirmCompanyName", YesNoAnswer.values)
          companyDetailsConfirmed.fold(emptyForm)(emptyForm.fill)
        }
        Future.successful(getConfirmCompanyDetailsPage(form, companyHouseName))
      }
    }
  }

  val confirmCompanyDetailsSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      def callUpdateAndNext(updatedSession: HECSession) =
        journeyService
          .updateAndNext(
            routes.CompanyDetailsController.confirmCompanyDetails,
            updatedSession
          )

      def fetchCTStatus(
        desCtutrOpt: Option[CTUTR],
        companyLoginData: CompanyLoginData,
        crn: CRN
      ): EitherT[Future, Error, Option[CTStatusResponse]] =
        companyLoginData.ctutr flatTraverse [EitherT[Future, Error, *], CTStatusResponse] { ctutr =>
          desCtutrOpt match {
            case Some(desCtutr) if desCtutr.value === ctutr.value =>
              auditService.sendEvent(
                EnrolmentCTUTRCompanyMatchSuccess(
                  crn,
                  desCtutr,
                  ctutr,
                  request.language,
                  companyLoginData.ggCredId
                )
              )
              val (start, end) = CompanyDetailsController.calculateLookBackPeriod(timeProvider.currentDate)
              taxCheckService.getCTStatus(desCtutr, start, end)

            case _ =>
              desCtutrOpt
                .foreach(desCtutr =>
                  auditService.sendEvent(
                    EnrolmentCTUTRCompanyMatchFailure(crn, desCtutr, ctutr, request.language, companyLoginData.ggCredId)
                  )
                )
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
          ctStatusOpt         <- fetchCTStatus(desCtutr, session.loginData, crn)
          updatedRetrievedData = session.retrievedJourneyData.copy(desCtutr = desCtutr, ctStatus = ctStatusOpt)
          updatedUserAnswers   = session.userAnswers
                                   .unset(_.companyDetailsConfirmed)
                                   .copy(companyDetailsConfirmed = Some(companyDetailsConfirmed))
          updatedSession       = session.copy(retrievedJourneyData = updatedRetrievedData, userAnswers = updatedUserAnswers)
          call                <- callUpdateAndNext(updatedSession)
        } yield call

        result.fold(
          _.doThrow("Could not update session and proceed"),
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
            val answersWithoutCrn = session.userAnswers
              .unset(_.crn)
              .unset(_.companyDetailsConfirmed)
              .copy(companyDetailsConfirmed = Some(companyDetailsConfirmed))
            callUpdateAndNext(session.copy(userAnswers = answersWithoutCrn)).fold(
              _.doThrow("Could not update session and proceed"),
              Redirect
            )
        }

      def getFuturePage(companyHouseName: CompanyHouseName)(form: Form[YesNoAnswer])(implicit
        request: RequestWithSessionData[_]
      ) =
        Future.successful(getConfirmCompanyDetailsPage(form, companyHouseName))

      request.sessionData mapAsCompany { companySession =>
        ensureCompanyDataHasCompanyName(companySession) { companyHouseName =>
          ensureUserAnswersHasCRN(companySession) { crn =>
            CompanyDetailsController
              .yesNoForm("confirmCompanyName", YesNoAnswer.values)
              .bindFromRequest()
              .fold(getFuturePage(companyHouseName), handleValidAnswer(companySession, crn))
          }
        }
      }
    }

  val recentlyStartedTrading: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData mapAsCompany { companySession =>
        val recentlyStartedTrading =
          companySession.userAnswers.fold(_.recentlyStartedTrading, _.recentlyStartedTrading)
        val form = {
          val emptyForm = CompanyDetailsController.yesNoForm("recentlyStartedTrading", YesNoAnswer.values)
          recentlyStartedTrading.fold(emptyForm)(emptyForm.fill)
        }
        Ok(
          recentlyStartedTradingPage(
            form = form,
            back = journeyService.previous(routes.CompanyDetailsController.recentlyStartedTrading),
            options = YesNoOption.yesNoOptions
          )
        )
      }
    }

  val recentlyStartedTradingSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData mapAsCompany { companySession =>
        def handleValidAnswer(recentlyStartedTrading: YesNoAnswer) = {
          val updatedAnswers = companySession.userAnswers
            .unset(_.recentlyStartedTrading)
            .copy(recentlyStartedTrading = Some(recentlyStartedTrading))

          updateAndNextJourneyData(
            routes.CompanyDetailsController.recentlyStartedTrading,
            request.sessionData.mapAsCompany(_.copy(userAnswers = updatedAnswers))
          )
        }

        CompanyDetailsController
          .yesNoForm("recentlyStartedTrading", YesNoAnswer.values)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                recentlyStartedTradingPage(
                  form = formWithErrors,
                  back = journeyService.previous(routes.CompanyDetailsController.recentlyStartedTrading),
                  options = YesNoOption.yesNoOptions
                )
              ),
            handleValidAnswer
          )
      }
    }

  val chargeableForCorporationTax: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData mapAsCompany { companySession =>
        ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
          val chargeableForCT = companySession.userAnswers.fold(_.chargeableForCT, _.chargeableForCT)
          val endDateStr      = TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate)
          val form = {
            val emptyForm = CompanyDetailsController.yesNoForm("chargeableForCT", YesNoAnswer.values, List(endDateStr))
            chargeableForCT.fold(emptyForm)(emptyForm.fill)
          }
          Ok(
            chargeableForCTPage(
              form = form,
              back = journeyService.previous(routes.CompanyDetailsController.chargeableForCorporationTax),
              date = endDateStr,
              options = YesNoOption.yesNoOptions
            )
          )
        }
      }
    }

  val chargeableForCorporationTaxSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData mapAsCompany { companySession =>
        def handleValidAnswer(chargeableForCT: YesNoAnswer) = {
          val updatedAnswers = {
            val existingAnswer = companySession.userAnswers.fold(_.chargeableForCT, _.chargeableForCT)
            if (existingAnswer.contains(chargeableForCT)) companySession.userAnswers
            else
              companySession.userAnswers
                .unset(_.chargeableForCT)
                .unset(_.ctIncomeDeclared)
                .copy(chargeableForCT = Some(chargeableForCT))
          }

          updateAndNextJourneyData(
            routes.CompanyDetailsController.chargeableForCorporationTax,
            companySession.copy(userAnswers = updatedAnswers)
          )
        }

        ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
          val endDateStr = TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate)
          CompanyDetailsController
            .yesNoForm("chargeableForCT", YesNoAnswer.values, List(endDateStr))
            .bindFromRequest()
            .fold(
              formWithErrors =>
                Ok(
                  chargeableForCTPage(
                    form = formWithErrors,
                    back = journeyService.previous(routes.CompanyDetailsController.chargeableForCorporationTax),
                    date = endDateStr,
                    options = YesNoOption.yesNoOptions
                  )
                ),
              handleValidAnswer
            )
        }
      }
    }

  val ctutrNotMatched: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.ctutrNotMatched)
    Ok(ctutrNotMatchedPage(back))
  }

  val enterCtutr: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData mapAsCompany { companySession =>
      ensureCompanyDataHasDesCtutr(companySession) { desCtutr =>
        val ctutr     = companySession.userAnswers.fold(_.ctutr, _.ctutr)
        val back      = journeyService.previous(routes.CompanyDetailsController.enterCtutr)
        val ctutrForm = enterCtutrForm(desCtutr)
        val form      = ctutr.fold(ctutrForm)(ctutrForm.fill)
        Ok(enterCtutrPage(form, back))
      }
    }
  }

  val enterCtutrSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData mapAsCompany { companySession =>
      ensureCompanyDataHasDesCtutr(companySession) { desCtutr =>
        ensureUserAnswersHasCRN(companySession) { crn =>
          ensureCompanyDataHasCompanyName(companySession) { companyName =>
            def handleValidAnswer(ctutr: CTUTR) = {
              val updatedAnswers = companySession.userAnswers
                .unset(_.ctutr)
                .unset(_.ctIncomeDeclared)
                .unset(_.recentlyStartedTrading)
                .unset(_.chargeableForCT)
                .copy(ctutr = Some(ctutr))
              val (start, end)   = CompanyDetailsController.calculateLookBackPeriod(timeProvider.currentDate)

              val ctutrAttemptsDeleteFut = ctutrAttemptsService.delete(crn, companySession.loginData.ggCredId)
              val ctStatusFut            = taxCheckService.getCTStatus(ctutr.strippedCtutr, start, end)

              val result = for {
                _                   <- ctutrAttemptsDeleteFut
                ctStatus            <- ctStatusFut
                updatedRetrievedData = companySession.retrievedJourneyData.copy(ctStatus = ctStatus)
                _                    = auditService
                                         .sendEvent(
                                           EnterCTUTRCompanyMatchSuccess(
                                             crn,
                                             ctutr,
                                             ctutr.strippedCtutr,
                                             desCtutr,
                                             request.language,
                                             companySession.loginData.ggCredId
                                           )
                                         )
                next                <-
                  journeyService.updateAndNext(
                    routes.CompanyDetailsController.enterCtutr,
                    companySession.copy(
                      userAnswers = updatedAnswers,
                      retrievedJourneyData = updatedRetrievedData,
                      crnBlocked = false
                    )
                  )
              } yield next

              result.fold(
                _.doThrow("Could not update session and proceed"),
                Redirect
              )
            }

            def ok(formWithErrors: Form[CTUTR]) = Ok(
              enterCtutrPage(
                formWithErrors,
                journeyService.previous(routes.CompanyDetailsController.enterCtutr)
              )
            )

            def handleFormWithErrors(formWithErrors: Form[CTUTR], ctutrAttempts: CtutrAttempts): Future[Result] = {

              def displayFormError: Future[Result] = Future.successful(ok(formWithErrors))

              def incrementAttemptsAndProceed =
                ctutrAttemptsService
                  .updateAttempts(ctutrAttempts)
                  .foldF(
                    _.doThrow("Could not create/update ctutr attempts"),
                    { ctutrAttempts =>
                      val submittedCTUTR = formWithErrors.data.getOrElse(enterCtutrFormKey, "")

                      auditService.sendEvent(
                        EnterCTUTRCompanyMatchFailure(
                          crn,
                          CTUTR(submittedCTUTR),
                          CTUTR(submittedCTUTR).strippedCtutr,
                          desCtutr,
                          ctutrAttempts.isBlocked,
                          request.language,
                          companySession.loginData.ggCredId
                        )
                      )

                      if (ctutrAttempts.isBlocked) {
                        auditService
                          .sendEvent(TaxCheckExit.CTEnteredCTUTRNotMatchingBlocked(companySession, request.language))
                        updateAndNextJourneyData(
                          routes.CompanyDetailsController.enterCtutr,
                          companySession.copy(crnBlocked = ctutrAttempts.isBlocked)
                        )
                      } else {
                        val updatedAnswers = companySession.userAnswers
                          .unset(_.ctutr)
                          .unset(_.ctIncomeDeclared)
                          .unset(_.recentlyStartedTrading)
                          .unset(_.chargeableForCT)
                          .copy(ctutr = formWithErrors.value)
                        val updatedSession = companySession
                          .copy(crnBlocked = ctutrAttempts.isBlocked, userAnswers = updatedAnswers)
                        sessionStore
                          .store(updatedSession)
                          .foldF(
                            _.doThrow("Could not save session"),
                            _ => displayFormError
                          )
                      }
                    }
                  )

              val ctutrsNotMatched = formWithErrors.errors.exists(_.message === "error.ctutrsDoNotMatch")
              if (ctutrsNotMatched) incrementAttemptsAndProceed else displayFormError
            }

            ctutrAttemptsService
              .getWithDefault(crn, companySession.loginData.ggCredId, companyName)
              .foldF(
                _.doThrow("Error fetching CTUTR attempts"),
                ctutrAttempts =>
                  if (ctutrAttempts.isBlocked) {
                    updateAndNextJourneyData(
                      routes.CompanyDetailsController.enterCtutr,
                      companySession.copy(crnBlocked = true)
                    )
                  } else {
                    enterCtutrForm(desCtutr)
                      .bindFromRequest()
                      .fold(handleFormWithErrors(_, ctutrAttempts), handleValidAnswer)
                  }
              )
          }
        }
      }
    }
  }

  val dontHaveUtr: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.dontHaveUtr)
    Ok(dontHaveCtutrPage(back))
  }

  val tooManyCtutrAttempts: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData mapAsCompany { companySession =>
      ensureUserAnswersHasCRN(companySession) { crn =>
        val back = journeyService.previous(routes.CompanyDetailsController.tooManyCtutrAttempts)

        ctutrAttemptsService
          .get(crn, companySession.loginData.ggCredId)
          .fold(
            _.doThrow("Error fetching ctutr attempts"),
            {
              case Some(CtutrAttempts(_, _, companyName, _, Some(blockedUntil))) =>
                val formattedDate =
                  TimeUtils.govDateTimeDisplayFormat(blockedUntil.withZoneSameInstant(ZoneId.of("Europe/London")))
                Ok(tooManyCTUTRAttemptsPage(back, crn.value, companyName.name, formattedDate))
              case _                                                             =>
                sys.error("CTUTR attempts is not blocked")
            }
          )
      }
    }
  }

  val ctIncomeStatement: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData mapAsCompany { companySession =>
      ensureCompanyDataHasCTStatusAccountingPeriod(companySession) { latestAccountingPeriod =>
        val back             = journeyService.previous(routes.CompanyDetailsController.ctIncomeStatement)
        val ctIncomeDeclared = companySession.userAnswers.fold(_.ctIncomeDeclared, _.ctIncomeDeclared)
        val form = {
          val emptyForm = CompanyDetailsController.yesNoForm("ctIncomeDeclared", YesNoAnswer.values)
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

          updateAndNextJourneyData(
            routes.CompanyDetailsController.ctIncomeStatement,
            companySession.copy(userAnswers = updatedAnswers)
          )
        }

        CompanyDetailsController
          .yesNoForm("ctIncomeDeclared", YesNoAnswer.values)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                ctIncomeStatementPage(
                  formWithErrors,
                  journeyService.previous(routes.CompanyDetailsController.ctIncomeStatement),
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
    val back = journeyService.previous(routes.CompanyDetailsController.cannotDoTaxCheck)
    Ok(cannotDoTaxCheckPage(back))
  }

  private def ensureCompanyDataHasCompanyName(
    companySession: CompanyHECSession
  )(f: CompanyHouseName => Future[Result]): Future[Result] =
    companySession.retrievedJourneyData.companyName match {
      case Some(companyName) => f(companyName)
      case None              =>
        sys.error("Missing company name")
    }

  private def ensureCompanyDataHasDesCtutr(
    companySession: CompanyHECSession
  )(f: CTUTR => Future[Result]): Future[Result] =
    companySession.retrievedJourneyData.desCtutr match {
      case Some(ctutr) => f(ctutr)
      case None        =>
        sys.error("Missing DES-CTUTR")
    }

  private def ensureCompanyDataHasCTStatusAccountingPeriod(
    companySession: CompanyHECSession
  )(f: CTAccountingPeriod => Future[Result]): Future[Result] =
    companySession.retrievedJourneyData.ctStatus match {
      case Some(CTStatusResponse(_, _, _, Some(latestAccountingPeriod))) => f(latestAccountingPeriod)
      case Some(_)                                                       =>
        sys.error("Missing CT status latest accounting period")
      case None                                                          =>
        sys.error("Missing CT status")
    }

  private def ensureUserAnswersHasCRN(
    session: CompanyHECSession
  )(f: CRN => Future[Result]): Future[Result] =
    session.userAnswers.fold(_.crn, _.crn.some) match {
      case Some(crn) => f(crn)
      case None      =>
        sys.error("CRN is not populated in user answers")
    }

  private def updateAndNextJourneyData(current: Call, updatedSession: HECSession)(implicit
    r: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): Future[Result] =
    journeyService
      .updateAndNext(
        current,
        updatedSession
      )
      .fold(
        _.doThrow("Could not update session and proceed"),
        Redirect
      )
}

object CompanyDetailsController {

  def yesNoForm(mappingName: String, options: List[YesNoAnswer], errorMsgArgs: List[String] = Nil): Form[YesNoAnswer] =
    Form(
      mapping(
        mappingName -> of(FormUtils.radioFormFormatter(options, errorMsgArgs))
      )(identity)(Some(_))
    )

  /**
    * Calculate the lookback period based on today's date.
    * The lookback period is the the most recent accounting period of the company to have ended 12 months or more
    * before the day on which the tax check is initiated. (These are the dates used when retrieving the Corporation tax
    * records for the Applicant's company using the Get Company Accounting Periods API.)
    */
  def calculateLookBackPeriod(today: LocalDate): (LocalDate, LocalDate) = {
    val currentDay = if (today.getMonth.getValue === 2 && today.getDayOfMonth === 29) today.plusDays(1) else today
    currentDay.minusYears(2) -> currentDay.minusYears(1).minusDays(1)
  }

  val enterCtutrFormKey: String = "enterCtutr"

  def enterCtutrForm(desCtrutr: CTUTR): Form[CTUTR] = {
    val validCtutr: Constraint[CTUTR] =
      Constraint { ctutr =>
        // using the stripped value here because the CTUTR validation checker only works with 10 digit UTRs
        CTUTR.fromString(ctutr.stripped) match {
          case Some(validCtutr) =>
            if (validCtutr.stripped === desCtrutr.value) Valid else Invalid("error.ctutrsDoNotMatch")
          case None             =>
            // if user input was already 10 digits, then checksum validation failed, otherwise the format was wrong
            if (ctutr.value.matches("""^\d{10}$""")) Invalid("error.ctutrChecksumFailed")
            else Invalid("error.ctutrInvalidFormat")
        }
      }

    Form(
      mapping(
        enterCtutrFormKey -> nonEmptyText
          .transform[CTUTR](
            s => CTUTR(s.removeWhitespace),
            _.value
          )
          .verifying(validCtutr)
      )(identity)(Some(_))
    )
  }
}
