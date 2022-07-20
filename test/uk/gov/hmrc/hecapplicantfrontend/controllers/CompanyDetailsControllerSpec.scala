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
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.inject.bind
import play.api.mvc.{Call, Cookie, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.CompanyDetailsController.calculateLookBackPeriod
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchFailure.{EnrolmentCTUTRCompanyMatchFailure, EnterCTUTRCompanyMatchFailure}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchSuccess.{EnrolmentCTUTRCompanyMatchSuccess, EnterCTUTRCompanyMatchSuccess}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckExit
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTAccountingPeriod.CTAccountingPeriodDigital
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.{CTStatus, CTStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, AuditServiceSupport, CtutrAttemptsService, JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.reflect.ClassTag

class CompanyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport
    with AuditServiceSupport {

  val mockTimeProvider         = mock[TimeProvider]
  val mockTaxCheckService      = mock[TaxCheckService]
  val mockCtutrAttemptsService = mock[CtutrAttemptsService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[TaxCheckService].toInstance(mockTaxCheckService),
    bind[CtutrAttemptsService].toInstance(mockCtutrAttemptsService),
    bind[AuditService].toInstance(mockAuditService)
  )

  def mockTimeProviderToday(d: LocalDate) = (mockTimeProvider.currentDate _).expects().returning(d)

  def mockTaxCheckServiceGetCtutr(crn: CRN)(result: Either[Error, Option[CTUTR]]) =
    (mockTaxCheckService
      .getCtutr(_: CRN)(_: HeaderCarrier))
      .expects(crn, *)
      .returning(EitherT.fromEither[Future](result))

  def mockTaxCheckServiceGetCtStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(
    result: Either[Error, Option[CTStatusResponse]]
  ) =
    (mockTaxCheckService
      .getCTStatus(_: CTUTR, _: LocalDate, _: LocalDate)(_: HeaderCarrier))
      .expects(ctutr, startDate, endDate, *)
      .returning(EitherT.fromEither[Future](result))

  def mockCtutrAttemptsServiceUpdateAttempts(attempts: CtutrAttempts)(
    result: Either[Error, CtutrAttempts]
  ) =
    (mockCtutrAttemptsService
      .updateAttempts(_: CtutrAttempts))
      .expects(attempts)
      .returning(EitherT.fromEither[Future](result))

  def mockCtutrAttemptsServiceGetWithDefault(crn: CRN, ggCredId: GGCredId, companyHouseName: CompanyHouseName)(
    result: Either[Error, CtutrAttempts]
  ) =
    (mockCtutrAttemptsService
      .getWithDefault(_: CRN, _: GGCredId, _: CompanyHouseName))
      .expects(crn, ggCredId, companyHouseName)
      .returning(EitherT.fromEither[Future](result))

  def mockCtutrAttemptsServiceGet(crn: CRN, ggCredId: GGCredId)(
    result: Either[Error, Option[CtutrAttempts]]
  ) =
    (mockCtutrAttemptsService
      .get(_: CRN, _: GGCredId))
      .expects(crn, ggCredId)
      .returning(EitherT.fromEither[Future](result))

  def mockCtutrAttemptsServiceDelete(crn: CRN, ggCredId: GGCredId)(result: Either[Error, Unit]) =
    (mockCtutrAttemptsService
      .delete(_: CRN, _: GGCredId))
      .expects(crn, ggCredId)
      .returning(EitherT.fromEither[Future](result))

  val controller                          = instanceOf[CompanyDetailsController]
  val companyLoginData                    = Fixtures.companyLoginData()
  val retrievedJourneyDataWithCompanyName =
    Fixtures.companyRetrievedJourneyData(companyName = Some(CompanyHouseName("some-company")))

  private lazy val maxCtutrAttempts = 3

  override def additionalConfig = super.additionalConfig.withFallback(
    Configuration(
      ConfigFactory.parseString(
        s"""
           | ctutr-attempts { maximum-attempts = $maxCtutrAttempts }
           | play.i18n.langs = ["en", "cy", "fr"]
           |""".stripMargin
      )
    )
  )

  "CompanyDetailsControllerSpec" when {

    "handling requests to the confirm company details page " must {

      def performAction(): Future[Result] = controller.confirmCompanyDetails(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        def test(session: HECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails, session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.confirmCompanyDetailsSubmit.url
            }
          )

        }

        "the user has not previously answered the question " in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName
          )
          test(session, None)
        }

        "the user has previously answered the question" in {
          val answers = Fixtures.completeCompanyUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            companyDetailsConfirmed = YesNoAnswer.Yes
          )
          val session = Fixtures.companyHECSession(companyLoginData, retrievedJourneyDataWithCompanyName, answers)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(companyDetailsConfirmed = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          test(updatedSession, Some("1"))

        }

      }

      "return a technical error" when {

        def testInconsistentSessionStateError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "company name is not populated" in {
          testInconsistentSessionStateError(Fixtures.companyHECSession())
        }

        "applicant is individual" in {
          testInconsistentSessionStateError(Fixtures.individualHECSession())

        }
      }
    }

    "handling submit on the confirm company name page" must {

      def performAction(data: (String, String)*)(language: Language): Future[Result] =
        controller.confirmCompanyDetailsSubmit(
          FakeRequest()
            .withMethod(POST)
            .withCookies(Cookie("PLAY_LANG", language.code))
            .withFormUrlEncodedBody(data: _*)
        )

      behave like authAndSessionDataBehaviour(() => performAction()(Language.English))

      "return a technical error" when {

        "the language is not recognised" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName,
            Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          val result = controller.confirmCompanyDetailsSubmit(
            FakeRequest().withMethod(POST).withCookies(Cookie("PLAY_LANG", "fr"))
          )
          a[RuntimeException] shouldBe thrownBy(await(result))
        }

      }

      "show a form error" when {

        val session = Fixtures.companyHECSession(
          companyLoginData,
          retrievedJourneyDataWithCompanyName,
          Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
        )

        def test(data: (String, String)*)(errorMessage: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails, session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(data: _*)(Language.English),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey(errorMessage)
          )
        }

        "nothing has been submitted" in {
          test()("confirmCompanyName.error.required")
        }

        "an invalid index value is submitted" in {
          test("confirmCompanyName" -> Int.MaxValue.toString)("confirmCompanyName.error.invalid")
        }

        "a non-numeric value is submitted" in {
          test("confirmCompanyName" -> "xyz")("confirmCompanyName.error.invalid")
        }
      }

      val date                 = LocalDate.now
      val (startDate, endDate) = calculateLookBackPeriod(date)

      "return a technical error" when {

        def testInconsistentSessionStateError(session: HECSession, value: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction("confirmCompanyName" -> value)(Language.English)))
        }

        "user answers with a Yes" when {

          "CRN is not populated" in {
            // session contains CTUTR from enrolments
            testInconsistentSessionStateError(
              Fixtures.companyHECSession(
                Fixtures.companyLoginData(ctutr = Some(CTUTR("ctutr"))),
                retrievedJourneyDataWithCompanyName
              ),
              "0"
            )

          }

          "the applicant type is individual" in {
            testInconsistentSessionStateError(Fixtures.individualHECSession(), "0")
          }

          "the call to fetch CT status fails" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val session = Fixtures.companyHECSession(
              Fixtures.companyLoginData(ctutr = Some(CTUTR("ctutr"))),
              retrievedJourneyDataWithCompanyName,
              answers
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(CTUTR("ctutr"))))
              mockSendAuditEvent(
                EnrolmentCTUTRCompanyMatchSuccess(
                  CRN("crn"),
                  CTUTR("ctutr"),
                  CTUTR("ctutr"),
                  Language.Welsh,
                  session.loginData.ggCredId
                )
              )
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(CTUTR("ctutr"), startDate, endDate)(
                Left(Error("fetch ct status failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "0")(Language.Welsh)))

          }

          "the call to update and next fails" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val ctutr   = CTUTR("ctutr")
            val session = Fixtures.companyHECSession(
              Fixtures.companyLoginData(ctutr = Some(ctutr)),
              retrievedJourneyDataWithCompanyName,
              answers
            )

            val updatedAnswers   = answers.copy(companyDetailsConfirmed = Some(YesNoAnswer.Yes))
            val ctStatusResponse = CTStatusResponse(ctutr, date, date, None)
            val updatedSession   = session.copy(
              userAnswers = updatedAnswers,
              retrievedJourneyData = retrievedJourneyDataWithCompanyName.copy(
                desCtutr = Some(ctutr),
                ctStatus = Some(ctStatusResponse)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(ctutr)))
              mockSendAuditEvent(
                EnrolmentCTUTRCompanyMatchSuccess(
                  CRN("crn"),
                  ctutr,
                  ctutr,
                  Language.English,
                  session.loginData.ggCredId
                )
              )
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(ctutr, startDate, endDate)(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails,
                session,
                updatedSession
              )(
                Left(Error("update and next failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "0")(Language.English)))

          }
        }

        "user answers with a No" when {

          "the call to update and next fails" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            val session = Fixtures.companyHECSession(companyLoginData, retrievedJourneyDataWithCompanyName, answers)

            // should wipe out CRN answer if user says that the company name is incorrect
            val updatedAnswers = answers.copy(crn = None, companyDetailsConfirmed = Some(YesNoAnswer.No))
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails,
                session,
                updatedSession
              )(Left(Error("some error")))
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "1")(Language.English)))

          }
        }

      }

      "redirect to the next page" when {

        "user answers with a Yes and all data fetches are successful" when {

          "the enrolment and DES CTUTRs match" when {

            def test(currentDate: LocalDate, lookBackStartDate: LocalDate, lookBackEndDate: LocalDate) = {
              val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
              // session contains CTUTR from enrolments
              val ctutr   = CTUTR("ctutr")
              val session = Fixtures.companyHECSession(
                Fixtures.companyLoginData(ctutr = Some(ctutr)),
                retrievedJourneyDataWithCompanyName,
                answers
              )

              val updatedAnswers   = answers.copy(companyDetailsConfirmed = Some(YesNoAnswer.Yes))
              val ctStatusResponse = CTStatusResponse(ctutr, currentDate, currentDate, None)
              val updatedSession   = session.copy(
                userAnswers = updatedAnswers,
                retrievedJourneyData = retrievedJourneyDataWithCompanyName.copy(
                  desCtutr = Some(ctutr),
                  ctStatus = Some(ctStatusResponse)
                )
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(ctutr)))
                mockSendAuditEvent(
                  EnrolmentCTUTRCompanyMatchSuccess(
                    CRN("crn"),
                    ctutr,
                    ctutr,
                    Language.English,
                    session.loginData.ggCredId
                  )
                )
                mockTimeProviderToday(currentDate)
                mockTaxCheckServiceGetCtStatus(ctutr, lookBackStartDate, lookBackEndDate)(
                  Right(Some(ctStatusResponse))
                )
                mockJourneyServiceUpdateAndNext(
                  routes.CompanyDetailsController.confirmCompanyDetails,
                  session,
                  updatedSession
                )(Right(mockNextCall))
              }

              checkIsRedirect(performAction("confirmCompanyName" -> "0")(Language.English), mockNextCall)
            }

            "today is 7 april 2021 " in {
              val currentDate       = LocalDate.of(2021, 4, 7)
              val lookBackStartDate = LocalDate.of(2019, 4, 7)
              val lookBackEndDate   = LocalDate.of(2020, 4, 6)
              test(currentDate, lookBackStartDate, lookBackEndDate)
            }

            "today is 1 march 2022 " in {
              val currentDate       = LocalDate.of(2022, 3, 1)
              val lookBackStartDate = LocalDate.of(2020, 3, 1)
              val lookBackEndDate   = LocalDate.of(2021, 2, 28)
              test(currentDate, lookBackStartDate, lookBackEndDate)
            }

            "today is  1 march 2024(leap year) " in {
              val currentDate       = LocalDate.of(2024, 3, 1)
              val lookBackStartDate = LocalDate.of(2022, 3, 1)
              val lookBackEndDate   = LocalDate.of(2023, 2, 28)
              test(currentDate, lookBackStartDate, lookBackEndDate)
            }

            "today is  29 feb 2024(leap year) " in {
              val currentDate       = LocalDate.of(2024, 2, 29)
              val lookBackStartDate = LocalDate.of(2022, 3, 1)
              val lookBackEndDate   = LocalDate.of(2023, 2, 28)

              test(currentDate, lookBackStartDate, lookBackEndDate)
            }

            "today is  1 march 2021 (1 year after leap year)  " in {
              val currentDate       = LocalDate.of(2021, 3, 1)
              val lookBackStartDate = LocalDate.of(2019, 3, 1)
              val lookBackEndDate   = LocalDate.of(2020, 2, 29)
              test(currentDate, lookBackStartDate, lookBackEndDate)
            }

          }

          "the enrolment and DES CTUTRs do not match" in {
            val answers     = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val companyData = companyLoginData.copy(ctutr = Some(CTUTR("ctutr")))
            val session     =
              Fixtures.companyHECSession(companyData, retrievedJourneyDataWithCompanyName, answers)

            val updatedAnswers = answers.copy(companyDetailsConfirmed = Some(YesNoAnswer.Yes))
            val desCtutr       = CTUTR("des-ctutr")
            val updatedSession = session.copy(
              userAnswers = updatedAnswers,
              retrievedJourneyData = retrievedJourneyDataWithCompanyName.copy(desCtutr = Some(desCtutr))
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(desCtutr)))
              mockSendAuditEvent(
                EnrolmentCTUTRCompanyMatchFailure(
                  CRN("crn"),
                  desCtutr,
                  CTUTR("ctutr"),
                  Language.Welsh,
                  session.loginData.ggCredId
                )
              )
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails,
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("confirmCompanyName" -> "0")(Language.Welsh), mockNextCall)
          }
        }

        "user answers with a No" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(companyLoginData, retrievedJourneyDataWithCompanyName, answers)

          // should wipe out CRN answer if user says that the company name is incorrect
          val updatedAnswers = answers.copy(crn = None, companyDetailsConfirmed = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("confirmCompanyName" -> "1")(Language.English), mockNextCall)
        }

      }

    }

    "handling requests to the chargeable for CT page " must {

      val date                                             = LocalDate.of(2020, 10, 5)
      val (lookBackPeriodStartDate, lookBackPeriodEndDate) = CompanyDetailsController.calculateLookBackPeriod(date)
      val ctStatusResponse                                 =
        CTStatusResponse(
          CTUTR("utr"),
          lookBackPeriodStartDate,
          lookBackPeriodEndDate,
          Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
        )

      def performAction(): Future[Result] = controller.chargeableForCorporationTax(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      behave like ensureLatestCTStatusBehaviour(
        performAction,
        routes.CompanyDetailsController.chargeableForCorporationTax,
        ctStatusResponse,
        date
      )

      "display the page" when {

        def test(session: CompanyHECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(date)
            if (session.newRelevantAccountingPeriodConsidered.contains(true))
              mockStoreSession(session.copy(newRelevantAccountingPeriodConsidered = None))(Right(()))
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.chargeableForCorporationTax, session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("chargeableForCT.title", "5 October 2020"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val newRelevantAccountingPeriodNotification = doc.select(".govuk-notification-banner__content")
              val expectedNotificationText                =
                if (session.newRelevantAccountingPeriodConsidered.contains(true))
                  messageFromMessageKey("newRelevantAccountingPeriod.notification")
                else ""

              newRelevantAccountingPeriodNotification.text() shouldBe expectedNotificationText

              val selectedOptions = doc.select(".govuk-radios__input[checked]")

              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.chargeableForCorporationTaxSubmit.url
            }
          )
        }

        val companyData = retrievedJourneyDataWithCompanyName.copy(ctStatus = Some(ctStatusResponse))

        "the user has not previously answered the question" in {
          val session =
            Fixtures.companyHECSession(companyLoginData, companyData, CompanyUserAnswers.empty)

          test(session, None)
        }

        "the user has previously answered the question" in {
          val answers = Fixtures.completeCompanyUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            chargeableForCT = Some(YesNoAnswer.Yes),
            ctIncomeDeclared = Some(YesNoAnswer.No)
          )
          val session = Fixtures.companyHECSession(companyLoginData, companyData, answers)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(chargeableForCT = Some(YesNoAnswer.Yes))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          test(updatedSession, Some("0"))

        }

        "a new relevant income tax year has been considered" in {
          val session =
            Fixtures.companyHECSession(
              companyLoginData,
              companyData,
              CompanyUserAnswers.empty,
              newRelevantAccountingPeriodConsidered = Some(true)
            )

          test(session, None)
        }

      }

      "return a technical error" when {

        "applicant is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "CT status accounting period is not populated" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(CTStatusResponse(CTUTR("utr"), lookBackPeriodStartDate, lookBackPeriodEndDate, None))
            ),
            CompanyUserAnswers.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(date)
          }
          assertThrows[InconsistentSessionState](await(performAction()))

        }
      }
    }

    "handling submit on the chargeable for CT page" must {

      val chargeableForCTRoute = routes.CompanyDetailsController.chargeableForCorporationTax
      val date                 = LocalDate.of(2020, 10, 5)
      val validJourneyData     = retrievedJourneyDataWithCompanyName.copy(
        ctStatus = Some(
          CTStatusResponse(CTUTR("utr"), date, date, Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound)))
        )
      )

      def performAction(data: (String, String)*): Future[Result] =
        controller.chargeableForCorporationTaxSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {
        val session = Fixtures.companyHECSession(
          companyLoginData,
          validJourneyData,
          Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
        )

        def test(formAnswer: (String, String)*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(chargeableForCTRoute, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formAnswer: _*),
            messageFromMessageKey("chargeableForCT.title", "5 October 2020"),
            messageFromMessageKey("chargeableForCT.error.required", "5 October 2020")
          )
        }

        "nothing has been submitted" in {
          test()
        }

        "an invalid index value is submitted" in {
          test("chargeableForCT" -> Int.MaxValue.toString)
        }

        "a non-numeric value is submitted" in {
          test("chargeableForCT" -> "xyz")
        }
      }

      "return a technical error" when {

        "the applicant type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "CT status accounting period is not populated" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(ctStatus = Some(CTStatusResponse(CTUTR("utr"), date, date, None)))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))

        }

        "the call to update and next fails" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(Fixtures.ctStatusResponse(latestAccountingPeriod = Some(Fixtures.ctAccountingPeriod())))
            ),
            answers
          )

          val updatedAnswers = answers.copy(chargeableForCT = Some(YesNoAnswer.Yes))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.CompanyDetailsController.chargeableForCorporationTax,
              session,
              updatedSession
            )(
              Left(Error("update and next failed"))
            )
          }
          assertThrows[RuntimeException](await(performAction("chargeableForCT" -> "0")))
        }

      }

      "redirect to the next page" when {

        "user gives a valid answer and" when {

          def test(currentSession: HECSession, updatedSession: HECSession, data: String) = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(currentSession)
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.chargeableForCorporationTax,
                currentSession,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("chargeableForCT" -> data), mockNextCall)
          }

          "the answer has not changed from an answer found in session" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(
              crn = Some(CRN("crn")),
              chargeableForCT = Some(YesNoAnswer.Yes),
              ctIncomeDeclared = Some(YesNoAnswer.No)
            )
            val session = Fixtures.companyHECSession(companyLoginData, validJourneyData, answers)

            test(session, session, "0")
          }

          "the answer has changed from an answer found in session" in {
            val answers = Fixtures.completeCompanyUserAnswers(
              crn = CRN("crn"),
              chargeableForCT = Some(YesNoAnswer.Yes),
              ctIncomeDeclared = Some(YesNoAnswer.No)
            )
            val session = Fixtures.companyHECSession(companyLoginData, validJourneyData, answers)

            val updatedAnswers = IncompleteCompanyUserAnswers
              .fromCompleteAnswers(answers)
              .copy(chargeableForCT = Some(YesNoAnswer.No), ctIncomeDeclared = None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            test(session, updatedSession, "1")
          }
        }

      }

    }

    "handling requests to the CT income statement page " must {

      val date                         = LocalDate.of(2020, 10, 5)
      val ctIncomeStatementRoute       = routes.CompanyDetailsController.ctIncomeStatement
      val ctIncomeStatementSubmitRoute = routes.CompanyDetailsController.ctIncomeStatementSubmit

      def performAction(): Future[Result] = controller.ctIncomeStatement(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {
        val companyData = retrievedJourneyDataWithCompanyName.copy(
          ctStatus = Some(
            CTStatusResponse(
              CTUTR("utr"),
              date,
              date,
              Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
            )
          )
        )

        def test(session: HECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(ctIncomeStatementRoute, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("ctIncomeDeclared.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              doc
                .select("#ctIncomeDeclared-hint")
                .text()                        shouldBe "This is your Company Tax Return for the accounting period ending 5 October 2020."

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val button = doc.select("form")
              button.attr("action") shouldBe ctIncomeStatementSubmitRoute.url
            }
          )
        }

        "the user has not previously answered the question " in {

          val session = Fixtures.companyHECSession(companyLoginData, companyData)
          test(session, None)

        }

        "the user has previously answered the question" in {

          val answers = Fixtures.completeCompanyUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            companyDetailsConfirmed = YesNoAnswer.Yes,
            chargeableForCT = Some(YesNoAnswer.No),
            ctIncomeDeclared = Some(YesNoAnswer.Yes)
          )
          val session = Fixtures.companyHECSession(companyLoginData, companyData, answers)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(ctIncomeDeclared = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          test(updatedSession, Some("1"))

        }

      }

      "return a technical error" when {

        def testInconsistentSessionStateError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "applicant is individual" in {
          testInconsistentSessionStateError(Fixtures.individualHECSession())
        }

        "CT status accounting period is not populated" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(Fixtures.ctStatusResponse(latestAccountingPeriod = None))
            )
          )
          testInconsistentSessionStateError(session)
        }
      }
    }

    "handling submit on the CT income statement page" must {

      val date                   = LocalDate.of(2020, 10, 5)
      val ctIncomeStatementRoute = routes.CompanyDetailsController.ctIncomeStatement
      val validJourneyData       = retrievedJourneyDataWithCompanyName.copy(
        ctStatus = Some(Fixtures.ctStatusResponse(latestAccountingPeriod = Some(Fixtures.ctAccountingPeriod())))
      )

      def performAction(data: (String, String)*): Future[Result] =
        controller.ctIncomeStatementSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {
        val session = Fixtures.companyHECSession(
          companyLoginData,
          validJourneyData,
          Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
        )

        def test(formAnswer: (String, String)*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(ctIncomeStatementRoute, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formAnswer: _*),
            messageFromMessageKey("ctIncomeDeclared.title"),
            messageFromMessageKey("ctIncomeDeclared.error.required")
          )
        }

        "nothing has been submitted" in {
          test()
        }

        "an invalid index value is submitted" in {
          test("ctIncomeDeclared" -> Int.MaxValue.toString)
        }

        "a non-numeric value is submitted" in {
          test("ctIncomeDeclared" -> "xyz")
        }
      }

      "return a technical error" when {

        def testInconsistentSessionStateError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "the applicant type is individual" in {
          testInconsistentSessionStateError(Fixtures.individualHECSession())
        }

        "CT status accounting period is not populated" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(ctStatus = Some(CTStatusResponse(CTUTR("utr"), date, date, None)))
          )
          testInconsistentSessionStateError(session)

        }

        "the call to update and next fails" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(
                Fixtures.ctStatusResponse(latestAccountingPeriod =
                  Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
                )
              )
            ),
            answers
          )

          val updatedAnswers = answers.copy(ctIncomeDeclared = Some(YesNoAnswer.Yes))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              ctIncomeStatementRoute,
              session,
              updatedSession
            )(
              Left(Error("update and next failed"))
            )
          }
          assertThrows[RuntimeException](await(performAction("ctIncomeDeclared" -> "0")))

        }

      }

      "redirect to the next page" when {

        "user gives a valid answer" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(companyLoginData, validJourneyData, answers)

          val updatedAnswers = answers.copy(ctIncomeDeclared = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              ctIncomeStatementRoute,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("ctIncomeDeclared" -> "1"), mockNextCall)
        }

      }

    }

    "handling requests to recently started trading page " must {

      val date                                             = LocalDate.of(2020, 10, 5)
      val (lookBackPeriodStartDate, lookBackPeriodEndDate) = CompanyDetailsController.calculateLookBackPeriod(date)
      val ctStatusResponse                                 = CTStatusResponse(
        CTUTR("utr"),
        lookBackPeriodStartDate,
        lookBackPeriodEndDate,
        None
      )

      val recentlyStartedTradingRoute       = routes.CompanyDetailsController.recentlyStartedTrading
      val recentlyStartedTradingSubmitRoute = routes.CompanyDetailsController.recentlyStartedTradingSubmit

      def performAction(): Future[Result] = controller.recentlyStartedTrading(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      behave like ensureLatestCTStatusBehaviour(
        performAction,
        routes.CompanyDetailsController.recentlyStartedTrading,
        ctStatusResponse,
        date
      )

      "display the page" when {
        val companyData = retrievedJourneyDataWithCompanyName.copy(ctStatus = Some(ctStatusResponse))

        def test(session: CompanyHECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(date)
            if (session.newRelevantAccountingPeriodConsidered.contains(true))
              mockStoreSession(session.copy(newRelevantAccountingPeriodConsidered = None))(Right(()))
            mockJourneyServiceGetPrevious(recentlyStartedTradingRoute, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("recentlyStartedTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val newRelevantAccountingPeriodNotification = doc.select(".govuk-notification-banner__content")
              val expectedNotificationText                =
                if (session.newRelevantAccountingPeriodConsidered.contains(true))
                  messageFromMessageKey("newRelevantAccountingPeriod.notification")
                else ""

              newRelevantAccountingPeriodNotification.text() shouldBe expectedNotificationText

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val button = doc.select("form")
              button.attr("action") shouldBe recentlyStartedTradingSubmitRoute.url
            }
          )
        }

        "the user has not previously answered the question " in {

          val session =
            Fixtures.companyHECSession(companyLoginData, companyData, CompanyUserAnswers.empty)
          test(session, None)

        }

        "the user has previously answered the question" in {

          val answers = Fixtures.completeCompanyUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            companyDetailsConfirmed = YesNoAnswer.Yes,
            recentlyStartedTrading = Some(YesNoAnswer.Yes)
          )
          val session = Fixtures.companyHECSession(companyLoginData, companyData, answers)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(recentlyStartedTrading = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          test(updatedSession, Some("1"))
        }

        "a new relevant income tax year has been considered" in {
          val session =
            Fixtures.companyHECSession(
              companyLoginData,
              companyData,
              CompanyUserAnswers.empty,
              newRelevantAccountingPeriodConsidered = Some(true)
            )

          test(session, None)
        }

      }

      "return a technical error" when {

        "applicant is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

      }
    }

    "handling submit on the recently started trading page" must {

      val date                        = LocalDate.of(2020, 10, 5)
      val recentlyStartedTradingRoute = routes.CompanyDetailsController.recentlyStartedTrading
      val validJourneyData            = retrievedJourneyDataWithCompanyName.copy(
        ctStatus = Some(
          CTStatusResponse(CTUTR("utr"), date, date, None)
        )
      )

      def performAction(data: (String, String)*): Future[Result] =
        controller.recentlyStartedTradingSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {
        val session = Fixtures.companyHECSession(
          companyLoginData,
          validJourneyData,
          CompanyUserAnswers.empty.copy(crn = Some(CRN("crn")))
        )

        def test(formAnswer: (String, String)*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(recentlyStartedTradingRoute, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formAnswer: _*),
            messageFromMessageKey("recentlyStartedTrading.title"),
            messageFromMessageKey("recentlyStartedTrading.error.required")
          )
        }

        "nothing has been submitted" in {
          test()
        }

        "an invalid index value is submitted" in {
          test("recentlyStartedTrading" -> Int.MaxValue.toString)
        }

        "a non-numeric value is submitted" in {
          test("recentlyStartedTrading" -> "xyz")
        }
      }

      "return a technical error" when {

        "the applicant type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "the call to update and next fails" in {
          val answers = CompanyUserAnswers.empty.copy(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(
                CTStatusResponse(
                  CTUTR("utr"),
                  date,
                  date,
                  Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
                )
              )
            ),
            answers
          )

          val updatedAnswers = answers.copy(recentlyStartedTrading = Some(YesNoAnswer.Yes))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              recentlyStartedTradingRoute,
              session,
              updatedSession
            )(
              Left(Error("update and next failed"))
            )
          }
          assertThrows[RuntimeException](await(performAction("recentlyStartedTrading" -> "0")))

        }

      }

      "redirect to the next page" when {

        "user gives a valid answer" in {
          val answers = CompanyUserAnswers.empty.copy(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(companyLoginData, validJourneyData, answers)

          val updatedAnswers = answers.copy(recentlyStartedTrading = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              recentlyStartedTradingRoute,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("recentlyStartedTrading" -> "1"), mockNextCall)
        }

      }

    }

    "handling requests to the enter CTUTR page " must {

      val enterCtutrRoute       = routes.CompanyDetailsController.enterCtutr
      val enterCtutrSubmitRoute = routes.CompanyDetailsController.enterCtutrSubmit

      def performAction(): Future[Result] = controller.enterCtutr(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {
        val companyData = retrievedJourneyDataWithCompanyName.copy(
          desCtutr = Some(CTUTR("utr"))
        )

        "the user has not previously answered the question " in {
          val session = Fixtures.companyHECSession(companyLoginData, companyData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(enterCtutrRoute, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enterCtutr.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              doc
                .select("#enterCtutr-hint")
                .text()                        shouldBe messageFromMessageKey("enterCtutr.hint")

              val input = doc.select("#enterCtutr")
              input.text() shouldBe ""

              val button = doc.select("form")
              button.attr("action") shouldBe enterCtutrSubmitRoute.url
            }
          )

        }

        "the user has previously answered the question" in {
          val ctutr   = "1111111111"
          val answers = Fixtures.completeCompanyUserAnswers(
            ctutr = Some(CTUTR(ctutr))
          )
          val session = Fixtures.companyHECSession(companyLoginData, companyData, answers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(enterCtutrRoute, session)(mockPreviousCall)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enterCtutr.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              doc
                .select("#enterCtutr-hint")
                .text()                        shouldBe messageFromMessageKey("enterCtutr.hint")

              val input = doc.select("#enterCtutr")
              input.attr("value") shouldBe ctutr

              val button = doc.select("form")
              button.attr("action") shouldBe enterCtutrSubmitRoute.url
            }
          )

        }

      }

      "return technical error" when {
        "applicant is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "DES CTUTR is not populated" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(desCtutr = None)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }
      }
    }

    "handling submit on enter ctutr page" must {

      val ctutr1          = "1111111111"
      val ctutr2          = "2222222222"
      val enterCtutrRoute = routes.CompanyDetailsController.enterCtutr
      val crn             = CRN("crn")
      val companyName     = CompanyHouseName("test company")

      def performAction(data: (String, String)*)(language: Language): Future[Result] =
        controller.enterCtutrSubmit(
          FakeRequest()
            .withMethod(POST)
            .withCookies(Cookie("PLAY_LANG", language.code))
            .withFormUrlEncodedBody(data: _*)
        )

      behave like authAndSessionDataBehaviour(() => performAction()(Language.English))

      "show a form error" when {
        val userAnswers   = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
        val session       = Fixtures.companyHECSession(
          companyLoginData,
          retrievedJourneyData =
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
          userAnswers = userAnswers
        )
        val ctutrAttempts = CtutrAttempts(crn, GGCredId("ggCredId"), companyName, 1, None)

        def test(errorMessageKey: String, formAnswer: (String, String)*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGetWithDefault(crn, session.loginData.ggCredId, companyName)(Right(ctutrAttempts))
            mockJourneyServiceGetPrevious(enterCtutrRoute, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formAnswer: _*)(Language.English),
            messageFromMessageKey("enterCtutr.title"),
            messageFromMessageKey(errorMessageKey)
          )
        }

        "nothing has been submitted" in {
          test("enterCtutr.error.required")
        }

        "CTUTR length is less than 10 digits long" in {
          test("enterCtutr.error.ctutrInvalidFormat", "enterCtutr" -> "111")
          test("enterCtutr.error.ctutrInvalidFormat", "enterCtutr" -> "111111111")
        }

        "CTUTR length is more than 10 digits long" in {
          test("enterCtutr.error.ctutrInvalidFormat", "enterCtutr" -> "11111111111")
        }

        "CTUTR contains letters" in {
          test("enterCtutr.error.ctutrInvalidFormat", "enterCtutr" -> "111111111a")
        }

        "CTUTR fails checksum validation" in {
          test("enterCtutr.error.ctutrChecksumFailed", "enterCtutr" -> "1234567890")
        }

        "input CTUTR does not match DES CTUTR & CRN is not blocked" in {
          val crn            = CRN("crn")
          val userAnswers    = Fixtures.incompleteCompanyUserAnswers(
            crn = Some(crn),
            ctutr = Some(CTUTR("")),
            ctIncomeDeclared = Some(YesNoAnswer.Yes),
            recentlyStartedTrading = Some(YesNoAnswer.Yes),
            chargeableForCT = Some(YesNoAnswer.No)
          )
          val session        = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyData =
              Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
            userAnswers = userAnswers,
            crnBlocked = true
          )
          val updatedSession = session.copy(
            userAnswers = userAnswers
              .copy(ctutr = None, ctIncomeDeclared = None, recentlyStartedTrading = None, chargeableForCT = None),
            crnBlocked = false
          )

          val ggCredId           = session.loginData.ggCredId
          val attempts           = CtutrAttempts(crn, ggCredId, companyName, 1, None)
          val submittedCTUTR     = CTUTR("2222222222")
          val expectedAuditEvent = EnterCTUTRCompanyMatchFailure(
            crn,
            submittedCTUTR,
            submittedCTUTR.strippedCtutr,
            CTUTR(ctutr1),
            false,
            Language.English,
            session.loginData.ggCredId
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGetWithDefault(crn, session.loginData.ggCredId, companyName)(Right(attempts))
            mockCtutrAttemptsServiceUpdateAttempts(attempts)(
              Right(CtutrAttempts(crn, ggCredId, companyName, 1, None))
            )
            mockSendAuditEvent(expectedAuditEvent)
            mockStoreSession(updatedSession)(Right(()))
            mockJourneyServiceGetPrevious(enterCtutrRoute, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("enterCtutr" -> submittedCTUTR.value)(Language.English),
            messageFromMessageKey("enterCtutr.title"),
            messageFromMessageKey("enterCtutr.error.ctutrsDoNotMatch")
          )
        }
      }

      "return a technical error" when {

        val attempts = CtutrAttempts(crn, GGCredId("ggCredId"), companyName, 1, None)

        "the applicant type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[InconsistentSessionState](await(performAction()(Language.English)))
        }

        "CRN answer is missing" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = None),
            userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = None)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction("enterCtutr" -> "some-utr")(Language.English)))
        }

        "DES CTUTR is not in the session" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = None),
            userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction("enterCtutr" -> "some-utr")(Language.English)))
        }

        val companySessionWithCrn = Fixtures.companyHECSession(
          companyLoginData,
          Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
          userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
        )

        "the call to fetch ctutr attempts fails" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(companySessionWithCrn)
            mockCtutrAttemptsServiceGetWithDefault(crn, companySessionWithCrn.loginData.ggCredId, companyName)(
              Left(Error("some error"))
            )
          }
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)(Language.English)))
        }

        "the call to delete ctutr attempts fails" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(companySessionWithCrn)
            mockCtutrAttemptsServiceGetWithDefault(crn, companySessionWithCrn.loginData.ggCredId, companyName)(
              Right(attempts)
            )
            mockTimeProviderToday(LocalDate.now)
            mockCtutrAttemptsServiceDelete(crn, companySessionWithCrn.loginData.ggCredId)(Left(Error("some error")))
          }
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)(Language.English)))
        }

        "the call to fetch CT status fails" in {
          val today                   = LocalDate.of(2021, 1, 13)
          val lookbackPeriodStartDate = today.minusYears(2)
          val lookbackPeriodEndDate   = today.minusYears(1).minusDays(1)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(companySessionWithCrn)
            mockCtutrAttemptsServiceGetWithDefault(crn, companySessionWithCrn.loginData.ggCredId, companyName)(
              Right(attempts)
            )
            mockTimeProviderToday(today)
            mockCtutrAttemptsServiceDelete(crn, companySessionWithCrn.loginData.ggCredId)(Right(()))
            mockTaxCheckServiceGetCtStatus(CTUTR(ctutr1), lookbackPeriodStartDate, lookbackPeriodEndDate)(
              Left(Error("fetch CT status failed"))
            )
          }
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)(Language.English)))
        }

        "the call to update and next fails" when {

          "user answer is valid" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            val session = Fixtures.companyHECSession(
              companyLoginData,
              Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
              userAnswers = answers
            )

            val today                   = LocalDate.of(2021, 1, 13)
            val lookbackPeriodStartDate = today.minusYears(2)
            val lookbackPeriodEndDate   = today.minusYears(1).minusDays(1)
            val ctStatusResponse        = CTStatusResponse(CTUTR(ctutr1), today, today, None)

            val updatedAnswers       = answers.copy(ctutr = Some(CTUTR(ctutr1)))
            val updatedRetrievedData = session.retrievedJourneyData.copy(ctStatus = Some(ctStatusResponse))
            val updatedSession       = session.copy(userAnswers = updatedAnswers, retrievedJourneyData = updatedRetrievedData)
            val expectedAuditEvent   =
              EnterCTUTRCompanyMatchSuccess(
                crn,
                CTUTR(ctutr1),
                CTUTR(ctutr1).strippedCtutr,
                CTUTR(ctutr1),
                Language.Welsh,
                session.loginData.ggCredId
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockCtutrAttemptsServiceGetWithDefault(crn, session.loginData.ggCredId, companyName)(
                Right(CtutrAttempts(crn, GGCredId("ggCredId"), companyName, 1, None))
              )
              mockTimeProviderToday(today)
              mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
              mockTaxCheckServiceGetCtStatus(CTUTR(ctutr1), lookbackPeriodStartDate, lookbackPeriodEndDate)(
                Right(Some(ctStatusResponse))
              )
              mockSendAuditEvent(expectedAuditEvent)
              mockJourneyServiceUpdateAndNext(
                enterCtutrRoute,
                session,
                updatedSession
              )(
                Left(Error("update and next failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)(Language.Welsh)))
          }

          "user answer does not match DES CTUTR & CRN has not been blocked yet but gets blocked on this attempt" in {
            List(
              Language.English,
              Language.Welsh
            ).foreach { lang =>
              withClue(s"For lang $lang: ") {

                val session = Fixtures.companyHECSession(
                  companyLoginData,
                  Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
                  crnBlocked = true,
                  userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
                )

                val attempts = CtutrAttempts(crn, companyLoginData.ggCredId, companyName, 1, None)

                val expectedMatchFailureAuditEvent =
                  EnterCTUTRCompanyMatchFailure(
                    crn,
                    CTUTR(ctutr2),
                    CTUTR(ctutr2).strippedCtutr,
                    CTUTR(ctutr1),
                    true,
                    lang,
                    session.loginData.ggCredId
                  )

                val expectedTaxCheckExitAuditEvent =
                  TaxCheckExit.CTEnteredCTUTRNotMatchingBlocked(session, lang)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockCtutrAttemptsServiceGetWithDefault(crn, companyLoginData.ggCredId, companyName)(Right(attempts))
                  mockCtutrAttemptsServiceUpdateAttempts(attempts)(
                    Right(attempts.copy(blockedUntil = Some(ZonedDateTime.now)))
                  )
                  mockSendAuditEvent(expectedMatchFailureAuditEvent)
                  mockSendAuditEvent(expectedTaxCheckExitAuditEvent)
                  mockJourneyServiceUpdateAndNext(
                    enterCtutrRoute,
                    session,
                    session
                  )(
                    Left(Error("update and next failed"))
                  )
                }
                assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr2)(lang)))
              }

            }
          }
        }

        "the call to store session fails when ctutrs do not match" in {
          val crn         = CRN("crn")
          val userAnswers = Fixtures.incompleteCompanyUserAnswers(
            crn = Some(crn),
            ctutr = Some(CTUTR("")),
            ctIncomeDeclared = Some(YesNoAnswer.Yes),
            recentlyStartedTrading = Some(YesNoAnswer.Yes),
            chargeableForCT = Some(YesNoAnswer.No)
          )
          val session     = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyData =
              Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR("des")), companyName = Some(companyName)),
            userAnswers = userAnswers,
            crnBlocked = true
          )

          val ggCredId = session.loginData.ggCredId
          val attempts = CtutrAttempts(crn, ggCredId, companyName, 0, None)

          val updatedSession = session.copy(
            userAnswers = userAnswers
              .copy(ctutr = None, ctIncomeDeclared = None, recentlyStartedTrading = None, chargeableForCT = None),
            crnBlocked = false
          )

          val expectedAuditEvent =
            EnterCTUTRCompanyMatchFailure(
              crn,
              CTUTR(ctutr1),
              CTUTR(ctutr1).strippedCtutr,
              CTUTR("des"),
              false,
              Language.English,
              session.loginData.ggCredId
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGetWithDefault(crn, session.loginData.ggCredId, companyName)(Right(attempts))
            mockCtutrAttemptsServiceUpdateAttempts(attempts)(Right(attempts.copy(attempts = 1)))
            mockSendAuditEvent(expectedAuditEvent)
            mockStoreSession(updatedSession)(Left(Error("some error")))
          }

          assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)(Language.English)))
        }
      }

      "redirect to the next page" when {

        "user gives a valid answer" when {

          "CRN is not blocked" in {
            val _13DigitCtutr = s"111$ctutr1"
            List(
              ctutr1               -> ctutr1,
              s"k$ctutr1"          -> ctutr1,
              s"${ctutr1}k"        -> ctutr1,
              _13DigitCtutr        -> ctutr1,
              s"k${_13DigitCtutr}" -> ctutr1,
              s"${_13DigitCtutr}k" -> ctutr1
            ).foreach { case (ctutrAnswer, strippedCtutrAnswer) =>
              withClue(s"for CTUTR = $ctutrAnswer") {
                val answers                 = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
                val session                 = Fixtures.companyHECSession(
                  companyLoginData,
                  Fixtures.companyRetrievedJourneyData(
                    desCtutr = Some(CTUTR(strippedCtutrAnswer)),
                    companyName = Some(companyName)
                  ),
                  answers
                )
                val today                   = LocalDate.of(2021, 1, 13)
                val lookbackPeriodStartDate = today.minusYears(2)
                val lookbackPeriodEndDate   = today.minusYears(1).minusDays(1)
                val ctStatusResponse        = CTStatusResponse(CTUTR(strippedCtutrAnswer), today, today, None)

                val updatedAnswers       = answers.copy(ctutr = Some(CTUTR(ctutrAnswer)))
                val updatedRetrievedData = session.retrievedJourneyData.copy(ctStatus = Some(ctStatusResponse))
                val updatedSession       =
                  session.copy(userAnswers = updatedAnswers, retrievedJourneyData = updatedRetrievedData)

                val expectedAuditEvent =
                  EnterCTUTRCompanyMatchSuccess(
                    crn,
                    CTUTR(ctutrAnswer),
                    CTUTR(strippedCtutrAnswer),
                    CTUTR(strippedCtutrAnswer),
                    Language.English,
                    session.loginData.ggCredId
                  )

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockCtutrAttemptsServiceGetWithDefault(crn, companyLoginData.ggCredId, companyName)(
                    Right(CtutrAttempts(crn, companyLoginData.ggCredId, companyName, 1, None))
                  )
                  mockTimeProviderToday(today)
                  mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
                  mockTaxCheckServiceGetCtStatus(
                    CTUTR(strippedCtutrAnswer),
                    lookbackPeriodStartDate,
                    lookbackPeriodEndDate
                  )(
                    Right(Some(ctStatusResponse))
                  )
                  mockSendAuditEvent(expectedAuditEvent)
                  mockJourneyServiceUpdateAndNext(
                    enterCtutrRoute,
                    session,
                    updatedSession
                  )(Right(mockNextCall))
                }

                checkIsRedirect(performAction("enterCtutr" -> ctutrAnswer)(Language.English), mockNextCall)
              }
            }
          }
        }

        "CRN is blocked" in {
          val session        = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
            userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn)),
            crnBlocked = false
          )
          val updatedSession = session.copy(crnBlocked = true)

          val attempts = CtutrAttempts(crn, companyLoginData.ggCredId, companyName, 1, Some(ZonedDateTime.now))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGetWithDefault(crn, companyLoginData.ggCredId, companyName)(Right(attempts))
            mockJourneyServiceUpdateAndNext(
              enterCtutrRoute,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("enterCtutr" -> ctutr1)(Language.English), mockNextCall)
        }

        "user's answer and DES CTUTR do not match & CRN gets blocked on this attempt" in {
          val answers                        = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session                        = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1)), companyName = Some(companyName)),
            crnBlocked = true,
            userAnswers = answers
          )
          val ggCredId                       = session.loginData.ggCredId
          val attempts                       = CtutrAttempts(crn, ggCredId, companyName, maxCtutrAttempts, None)
          val updatedAttempts                = CtutrAttempts(crn, ggCredId, companyName, maxCtutrAttempts, Some(ZonedDateTime.now))
          val expectedMatchFailureAuditEvent =
            EnterCTUTRCompanyMatchFailure(
              crn,
              CTUTR(ctutr2),
              CTUTR(ctutr2).strippedCtutr,
              CTUTR(ctutr1),
              true,
              Language.Welsh,
              session.loginData.ggCredId
            )
          val expectedTaxCheckExitAuditEvent =
            TaxCheckExit.CTEnteredCTUTRNotMatchingBlocked(session, Language.Welsh)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGetWithDefault(crn, companyLoginData.ggCredId, companyName)(Right(attempts))
            mockCtutrAttemptsServiceUpdateAttempts(attempts)(Right(updatedAttempts))
            mockSendAuditEvent(expectedMatchFailureAuditEvent)
            mockSendAuditEvent(expectedTaxCheckExitAuditEvent)
            mockJourneyServiceUpdateAndNext(
              enterCtutrRoute,
              session,
              session
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("enterCtutr" -> ctutr2)(Language.Welsh), mockNextCall)
        }

      }

    }

    "handling requests to the 'don't have CTUTR' page" must {

      def performAction() = controller.dontHaveUtr(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {
        val session = Fixtures.companyHECSession()
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.CompanyDetailsController.dontHaveUtr, session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("dontHaveCtutr.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url
            val links = doc.select(".govuk-body > .govuk-link")
            links.iterator().asScala.toList.map(_.attr("href")) shouldBe List(
              appConfig.registerForCtUrl,
              appConfig.findLostUtrUrl,
              mockPreviousCall.url,
              routes.SignOutController.exitSurvey.url
            )
          }
        )

      }

    }

    "handling requests to the 'CT UTR not matched' page" must {

      def performAction() = controller.ctutrNotMatched(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {
        val session = Fixtures.companyHECSession()
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.CompanyDetailsController.ctutrNotMatched, session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("ctutrNotMatched.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url
            val links = doc.select(".govuk-body > .govuk-link")
            links.iterator().asScala.toList.map(_.attr("href")) shouldBe List(
              appConfig.signOutAndSignBackInUrl,
              appConfig.registerForNewGGAccountUrl(EntityType.Company)
            )
          }
        )

      }

    }

    "handling requests to the 'too many CT UTR attempts' page" must {

      def performAction() = controller.tooManyCtutrAttempts(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      val crn                  = CRN("crn")
      val companyName          = CompanyHouseName("test company")
      val ctutrAttempts        = CtutrAttempts(crn, GGCredId(""), companyName, 1, None)
      val blockedUntil         = ZonedDateTime.of(2020, 1, 1, 10, 5, 0, 0, ZoneId.of("Europe/London"))
      val blockedCtutrAttempts = ctutrAttempts.copy(blockedUntil = Some(blockedUntil))

      "display the page" in {
        val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
        val session = Fixtures.companyHECSession(userAnswers = answers)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.CompanyDetailsController.tooManyCtutrAttempts, session)(
            mockPreviousCall
          )
          mockCtutrAttemptsServiceGet(crn, companyLoginData.ggCredId)(Right(Some(blockedCtutrAttempts)))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("tooManyCtutrAttempts.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val rowValues = doc.select(".govuk-summary-list__value")
            rowValues.get(0).text shouldBe companyName.name
            rowValues.get(1).text shouldBe crn.value

            doc.select("p.govuk-body").get(1).text() shouldBe messageFromMessageKey(
              "tooManyCtutrAttempts.p2",
              "1 January 2020 10:05am"
            )

            val links = doc.select(".govuk-body > .govuk-link")
            links.get(0).attr("href") shouldBe routes.CRNController.companyRegistrationNumber.url
            links.get(1).attr("href") shouldBe appConfig.applicantServiceGuidanceUrl
          }
        )

      }

      "throw exception" when {
        def testInconsistentSessionStateError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "session is for individual" in {
          testInconsistentSessionStateError(Fixtures.individualHECSession())
        }

        "CRN is missing in session answers" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = None)
          val session = Fixtures.companyHECSession(userAnswers = answers)
          testInconsistentSessionStateError(session)
        }

        def testCtutrAttempts[E <: Exception : ClassTag](
          session: HECSession,
          result: Either[Error, Option[CtutrAttempts]]
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.tooManyCtutrAttempts, session)(
              mockPreviousCall
            )
            mockCtutrAttemptsServiceGet(crn, companyLoginData.ggCredId)(result)
          }

          assertThrows[E](await(performAction()))
        }

        "fetching ctutr attempts fails" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
          val session = Fixtures.companyHECSession(userAnswers = answers)
          testCtutrAttempts[RuntimeException](session, Left(Error("some error")))

        }

        "fetched ctutr attempts is not blocked" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
          val session = Fixtures.companyHECSession(userAnswers = answers)
          testCtutrAttempts[InconsistentSessionState](session, Right(Some(ctutrAttempts)))

        }
      }

    }

  }

  def ensureLatestCTStatusBehaviour(
    performAction: () => Future[Result],
    currentEndpoint: Call,
    successfulCtStatusResponse: CTStatusResponse,
    today: LocalDate
  ): Unit = {
    val yesterday =
      today.minusDays(1L)

    val (yesterdayLookBackPeriodStart, yesterdayLookBackPeriodEnd) =
      CompanyDetailsController.calculateLookBackPeriod(yesterday)

    val yesterdayCtStatusResponse =
      successfulCtStatusResponse.copy(
        startDate = yesterdayLookBackPeriodStart,
        endDate = yesterdayLookBackPeriodEnd
      )

    val (todayLookBackPeriodStart, todayLookBackPeriodEnd) = CompanyDetailsController.calculateLookBackPeriod(today)

    val todayCtStatusResponse =
      successfulCtStatusResponse.copy(
        startDate = todayLookBackPeriodStart,
        endDate = todayLookBackPeriodEnd
      )

    "return an error" when {

      "no ct status response can be found in session" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(
            Fixtures.companyHECSession(retrievedJourneyData = Fixtures.companyRetrievedJourneyData(ctStatus = None))
          )
        }

        an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
      }

      "a new CT status API response is not required and" when {

        "there is an error while resetting a 'newRelevantAccountingPeriodConsidered' flag in session" in {
          val session =
            Fixtures.companyHECSession(
              retrievedJourneyData = Fixtures.companyRetrievedJourneyData(ctStatus = Some(todayCtStatusResponse)),
              newRelevantAccountingPeriodConsidered = Some(true)
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(today)
            mockStoreSession(session.copy(newRelevantAccountingPeriodConsidered = None))(Left(Error("")))
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction()))
        }

      }

      "a new CT status API response is required and" when {

        "the call to get a new CT status API response fails" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(
              Fixtures.companyHECSession(retrievedJourneyData =
                Fixtures.companyRetrievedJourneyData(ctStatus = Some(yesterdayCtStatusResponse))
              )
            )
            mockTimeProviderToday(today)
            mockTaxCheckServiceGetCtStatus(
              successfulCtStatusResponse.ctutr,
              todayLookBackPeriodStart,
              todayLookBackPeriodEnd
            )(Left(Error("")))
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction()))
        }

        "updating the session fails" in {
          val answers = Fixtures.completeCompanyUserAnswers(
            chargeableForCT = Some(YesNoAnswer.Yes),
            recentlyStartedTrading = Some(YesNoAnswer.No),
            ctIncomeDeclared = Some(YesNoAnswer.Yes)
          )
          val session =
            Fixtures.companyHECSession(
              retrievedJourneyData = Fixtures.companyRetrievedJourneyData(
                ctStatus = Some(
                  yesterdayCtStatusResponse.copy(
                    latestAccountingPeriod = None
                  )
                )
              ),
              userAnswers = answers
            )

          val updatedCtStatusResponse =
            todayCtStatusResponse.copy(
              latestAccountingPeriod =
                Some(CTAccountingPeriodDigital(LocalDate.MIN, LocalDate.MAX, CTStatus.NoticeToFileIssued))
            )

          val updatedSession =
            session.copy(
              userAnswers = IncompleteCompanyUserAnswers(
                Some(answers.licenceType),
                Some(answers.licenceTimeTrading),
                Some(answers.licenceValidityPeriod),
                answers.entityType,
                Some(answers.crn),
                Some(answers.companyDetailsConfirmed),
                None,
                None,
                None,
                answers.ctutr
              ),
              retrievedJourneyData = session.retrievedJourneyData.copy(ctStatus = Some(updatedCtStatusResponse)),
              newRelevantAccountingPeriodConsidered = Some(true)
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(today)
            mockTaxCheckServiceGetCtStatus(
              successfulCtStatusResponse.ctutr,
              todayLookBackPeriodStart,
              todayLookBackPeriodEnd
            )(Right(Some(updatedCtStatusResponse)))
            mockJourneyServiceUpdateAndNext(currentEndpoint, session, updatedSession)(Left(Error("")))
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction()))
        }
      }

    }

    "proceed without retrieving a new CT status response" when {

      "the look back period start and end dates have not changed and" when {

        val answers = Fixtures.incompleteCompanyUserAnswers(
          chargeableForCT = Some(YesNoAnswer.Yes),
          recentlyStartedTrading = Some(YesNoAnswer.No),
          ctIncomeDeclared = Some(YesNoAnswer.Yes)
        )

        val session =
          Fixtures.companyHECSession(
            userAnswers = answers,
            retrievedJourneyData = Fixtures.companyRetrievedJourneyData(ctStatus = Some(yesterdayCtStatusResponse))
          )

        "the 'newRelevantAccountingPeriodConsidered' in session is not defined" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session.copy(newRelevantAccountingPeriodConsidered = None))
            mockTimeProviderToday(yesterday)
            mockJourneyServiceGetPrevious(currentEndpoint, session)(mockPreviousCall)
          }

          status(performAction()) shouldBe OK
        }

        "the 'newRelevantAccountingPeriodConsidered' in session is false" in {
          val initialSession = session.copy(newRelevantAccountingPeriodConsidered = Some(false))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
            mockTimeProviderToday(yesterday)
            mockJourneyServiceGetPrevious(currentEndpoint, initialSession)(mockPreviousCall)
          }

          status(performAction()) shouldBe OK
        }

        "the 'newRelevantAccountingPeriodConsidered' in session is true" in {
          val initialSession = session.copy(newRelevantAccountingPeriodConsidered = Some(true))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(initialSession)
            mockTimeProviderToday(yesterday)
            mockStoreSession(initialSession.copy(newRelevantAccountingPeriodConsidered = None))(Right(()))
            mockJourneyServiceGetPrevious(currentEndpoint, initialSession)(mockPreviousCall)
          }

          status(performAction()) shouldBe OK
        }
      }

    }

    "retrieve a new CT status response and redirect to the next page" when {

      def test(storedStartDate: LocalDate, storedEndDate: LocalDate) = {
        val answers = Fixtures.incompleteCompanyUserAnswers(
          chargeableForCT = Some(YesNoAnswer.Yes),
          recentlyStartedTrading = Some(YesNoAnswer.No),
          ctIncomeDeclared = Some(YesNoAnswer.Yes)
        )

        val session =
          Fixtures.companyHECSession(
            userAnswers = answers,
            retrievedJourneyData = Fixtures.companyRetrievedJourneyData(ctStatus =
              Some(
                successfulCtStatusResponse.copy(
                  startDate = storedStartDate,
                  endDate = storedEndDate
                )
              )
            )
          )

        val updatedSession =
          session.copy(
            userAnswers = answers.copy(
              chargeableForCT = None,
              recentlyStartedTrading = None,
              ctIncomeDeclared = None
            ),
            retrievedJourneyData = session.retrievedJourneyData.copy(ctStatus = Some(todayCtStatusResponse)),
            newRelevantAccountingPeriodConsidered = Some(false)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockTimeProviderToday(today)
          mockTaxCheckServiceGetCtStatus(
            successfulCtStatusResponse.ctutr,
            todayLookBackPeriodStart,
            todayLookBackPeriodEnd
          )(Right(Some(todayCtStatusResponse)))
          mockJourneyServiceUpdateAndNext(currentEndpoint, session, updatedSession)(Right(mockNextCall))
        }

        checkIsRedirect(performAction(), mockNextCall)
      }

      "the look back period start date has changed" in {
        test(yesterdayLookBackPeriodEnd, todayLookBackPeriodEnd)
      }

      "the look back period end date has changed" in {
        test(todayLookBackPeriodStart, yesterdayLookBackPeriodEnd)
      }

      "both the look back period start and end date has changed" in {
        test(yesterdayLookBackPeriodEnd, yesterdayLookBackPeriodEnd)
      }

      "the relevant accounting period has changed" in {
        val answers = Fixtures.incompleteCompanyUserAnswers()

        val session =
          Fixtures.companyHECSession(
            userAnswers = answers,
            retrievedJourneyData = Fixtures.companyRetrievedJourneyData(ctStatus =
              Some(
                successfulCtStatusResponse.copy(
                  startDate = yesterdayLookBackPeriodStart,
                  endDate = yesterdayLookBackPeriodEnd
                )
              )
            )
          )

        val updatedCtStatusResponse = todayCtStatusResponse.copy(
          latestAccountingPeriod = Some(CTAccountingPeriodDigital(LocalDate.MIN, LocalDate.MAX, CTStatus.ReturnFound))
        )

        val updatedSession =
          session.copy(
            userAnswers = answers,
            retrievedJourneyData = session.retrievedJourneyData.copy(ctStatus = Some(updatedCtStatusResponse)),
            newRelevantAccountingPeriodConsidered = Some(true)
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockTimeProviderToday(today)
          mockTaxCheckServiceGetCtStatus(
            successfulCtStatusResponse.ctutr,
            todayLookBackPeriodStart,
            todayLookBackPeriodEnd
          )(Right(Some(updatedCtStatusResponse)))
          mockJourneyServiceUpdateAndNext(currentEndpoint, session, updatedSession)(Right(mockNextCall))
        }

        checkIsRedirect(performAction(), mockNextCall)
      }

    }

  }

}
