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
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{await, defaultAwaitTimeout}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{CtutrAttemptsService, JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZonedDateTime}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CompanyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider         = mock[TimeProvider]
  val mockTaxCheckService      = mock[TaxCheckService]
  val mockCtutrAttemptsService = mock[CtutrAttemptsService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[TaxCheckService].toInstance(mockTaxCheckService),
    bind[CtutrAttemptsService].toInstance(mockCtutrAttemptsService)
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

  def mockCtutrAttemptsServiceCreateOrIncrementAttempts(crn: CRN, ggCredId: GGCredId)(
    result: Either[Error, CtutrAttempts]
  ) =
    (mockCtutrAttemptsService
      .createOrIncrementAttempts(_: CRN, _: GGCredId))
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
           | maximum-ctutr-answer-attempts = $maxCtutrAttempts
           |""".stripMargin
      )
    )
  )

  "CompanyDetailsControllerSpec" when {

    "handling requests to the confirm company details page " must {

      def performAction(): Future[Result] = controller.confirmCompanyDetails(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question " in {

          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.confirmCompanyDetailsSubmit().url
            }
          )

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

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )(mockPreviousCall)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "1"

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.confirmCompanyDetailsSubmit().url
            }
          )

        }

      }

      "return a technical error" when {

        "company name is not populated" in {
          val session = Fixtures.companyHECSession()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "applicant is individual" in {
          val session = Fixtures.individualHECSession()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[RuntimeException](await(performAction()))
        }
      }
    }

    "handling submit on the confirm company name page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.confirmCompanyDetailsSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = Fixtures.companyHECSession(
          companyLoginData,
          retrievedJourneyDataWithCompanyName,
          Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
        )

        "nothing has been submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey("confirmCompanyName.error.required")
          )
        }

        "an invalid index value is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("confirmCompanyName" -> Int.MaxValue.toString),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey("confirmCompanyName.error.invalid")
          )
        }

        "a non-numeric value is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("confirmCompanyName" -> "xyz"),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey("confirmCompanyName.error.invalid")
          )
        }
      }

      val date                 = LocalDate.now
      val (startDate, endDate) = (date.minusYears(2).plusDays(1), date.minusYears(1))

      "return a technical error" when {

        "user answers with a Yes" when {

          "CRN is not populated" in {
            // session contains CTUTR from enrolments
            val session = Fixtures.companyHECSession(
              Fixtures.companyLoginData(ctutr = Some(CTUTR("ctutr"))),
              retrievedJourneyDataWithCompanyName
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "0")))

          }

          "the applicant type is individual" in {
            val session = Fixtures.individualHECSession()

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "0")))
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
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(CTUTR("ctutr"), startDate, endDate)(
                Left(Error("fetch ct status failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "0")))

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
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(ctutr, startDate, endDate)(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(
                Left(Error("update and next failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "0")))

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
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(Left(Error("some error")))
            }
            assertThrows[RuntimeException](await(performAction("confirmCompanyName" -> "1")))

          }
        }

      }

      "redirect to the next page" when {

        "user answers with a Yes and all data fetches are successful" when {
          "the enrolment and DES CTUTRs match" in {
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
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(ctutr, startDate, endDate)(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("confirmCompanyName" -> "0"), mockNextCall)
          }

          "the enrolment and DES CTUTRs do not match" in {
            val answers     = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val companyData = companyLoginData.copy(ctutr = Some(CTUTR("ctutr")))
            val session     =
              CompanyHECSession(companyData, retrievedJourneyDataWithCompanyName, answers, None, None, List.empty)

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
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("confirmCompanyName" -> "0"), mockNextCall)
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
              routes.CompanyDetailsController.confirmCompanyDetails(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("confirmCompanyName" -> "1"), mockNextCall)
        }

      }

    }

    "handling requests to the chargeable for CT page " must {

      val date = LocalDate.of(2020, 10, 5)

      def performAction(): Future[Result] = controller.chargeableForCorporationTax(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {
        val companyData = retrievedJourneyDataWithCompanyName.copy(
          ctStatus = Some(
            CTStatusResponse(
              CTUTR("utr"),
              date,
              date,
              Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound))
            )
          )
        )

        "the user has not previously answered the question " in {

          val session =
            CompanyHECSession(companyLoginData, companyData, CompanyUserAnswers.empty, None, None, List.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.chargeableForCorporationTax(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("chargeableForCT.title", "5 October 2020"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.chargeableForCorporationTaxSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {

          val answers = Fixtures.completeCompanyUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            chargeableForCT = Some(YesNoAnswer.Yes),
            ctIncomeDeclared = Some(YesNoAnswer.No)
          )
          val session = CompanyHECSession(companyLoginData, companyData, answers, None, None, List.empty)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(chargeableForCT = Some(YesNoAnswer.Yes))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(
              routes.CompanyDetailsController.chargeableForCorporationTax(),
              updatedSession
            )(mockPreviousCall)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("chargeableForCT.title", "5 October 2020"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "0"

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.chargeableForCorporationTaxSubmit().url
            }
          )

        }

      }

      "return a technical error" when {

        "applicant is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[RuntimeException](await(performAction()))
        }

        "CT status accounting period is not populated" in {
          val session = CompanyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(CTStatusResponse(CTUTR("utr"), date, date, None))
            ),
            CompanyUserAnswers.empty,
            None,
            None,
            List.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))

        }
      }
    }

    "handling submit on the chargeable for CT page" must {

      val chargeableForCTRoute = routes.CompanyDetailsController.chargeableForCorporationTax()
      val date                 = LocalDate.of(2020, 10, 5)
      val validJourneyData     = retrievedJourneyDataWithCompanyName.copy(
        ctStatus = Some(
          CTStatusResponse(CTUTR("utr"), date, date, Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound)))
        )
      )

      def performAction(data: (String, String)*): Future[Result] =
        controller.chargeableForCorporationTaxSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

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

          assertThrows[RuntimeException](await(performAction()))
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
          assertThrows[RuntimeException](await(performAction()))

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
              routes.CompanyDetailsController.chargeableForCorporationTax(),
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

          "the answer has not changed from an answer found in session" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(
              crn = Some(CRN("crn")),
              chargeableForCT = Some(YesNoAnswer.Yes),
              ctIncomeDeclared = Some(YesNoAnswer.No)
            )
            val session = Fixtures.companyHECSession(companyLoginData, validJourneyData, answers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.chargeableForCorporationTax(),
                session,
                session
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("chargeableForCT" -> "0"), mockNextCall)
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

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.chargeableForCorporationTax(),
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("chargeableForCT" -> "1"), mockNextCall)
          }
        }

      }

    }

    "handling requests to the CT income statement page " must {

      val date                         = LocalDate.of(2020, 10, 5)
      val ctIncomeStatementRoute       = routes.CompanyDetailsController.ctIncomeStatement()
      val ctIncomeStatementSubmitRoute = routes.CompanyDetailsController.ctIncomeStatementSubmit()

      def performAction(): Future[Result] = controller.ctIncomeStatement(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {
        val companyData = retrievedJourneyDataWithCompanyName.copy(
          ctStatus = Some(
            CTStatusResponse(
              CTUTR("utr"),
              date,
              date,
              Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound))
            )
          )
        )

        "the user has not previously answered the question " in {

          val session = Fixtures.companyHECSession(companyLoginData, companyData)

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
              selectedOptions.isEmpty shouldBe true

              val button = doc.select("form")
              button.attr("action") shouldBe ctIncomeStatementSubmitRoute.url
            }
          )

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

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(ctIncomeStatementRoute, updatedSession)(mockPreviousCall)
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
              selectedOptions.attr("value") shouldBe "1"

              val button = doc.select("form")
              button.attr("action") shouldBe ctIncomeStatementSubmitRoute.url
            }
          )

        }

      }

      "return a technical error" when {

        "applicant is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[RuntimeException](await(performAction()))
        }

        "CT status accounting period is not populated" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(Fixtures.ctStatusResponse(latestAccountingPeriod = None))
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))

        }
      }
    }

    "handling submit on the CT income statement page" must {

      val date                   = LocalDate.of(2020, 10, 5)
      val ctIncomeStatementRoute = routes.CompanyDetailsController.ctIncomeStatement()
      val validJourneyData       = retrievedJourneyDataWithCompanyName.copy(
        ctStatus = Some(Fixtures.ctStatusResponse(latestAccountingPeriod = Some(Fixtures.ctAccountingPeriod())))
      )

      def performAction(data: (String, String)*): Future[Result] =
        controller.ctIncomeStatementSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

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

        "the applicant type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[RuntimeException](await(performAction()))
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
          assertThrows[RuntimeException](await(performAction()))

        }

        "the call to update and next fails" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(
                Fixtures.ctStatusResponse(latestAccountingPeriod =
                  Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound))
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

      val date                              = LocalDate.of(2020, 10, 5)
      val recentlyStartedTradingRoute       = routes.CompanyDetailsController.recentlyStartedTrading()
      val recentlyStartedTradingSubmitRoute = routes.CompanyDetailsController.recentlyStartedTradingSubmit()

      def performAction(): Future[Result] = controller.recentlyStartedTrading(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {
        val companyData = retrievedJourneyDataWithCompanyName.copy(
          ctStatus = Some(
            CTStatusResponse(
              CTUTR("utr"),
              date,
              date,
              None
            )
          )
        )

        "the user has not previously answered the question " in {

          val session =
            CompanyHECSession(companyLoginData, companyData, CompanyUserAnswers.empty, None, None, List.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(recentlyStartedTradingRoute, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("recentlyStartedTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val button = doc.select("form")
              button.attr("action") shouldBe recentlyStartedTradingSubmitRoute.url
            }
          )

        }

        "the user has previously answered the question" in {

          val answers = Fixtures.completeCompanyUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            companyDetailsConfirmed = YesNoAnswer.Yes,
            recentlyStartedTrading = Some(YesNoAnswer.Yes)
          )
          val session = CompanyHECSession(companyLoginData, companyData, answers, None, None, List.empty)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(recentlyStartedTrading = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(recentlyStartedTradingRoute, updatedSession)(mockPreviousCall)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("recentlyStartedTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "1"

              val button = doc.select("form")
              button.attr("action") shouldBe recentlyStartedTradingRoute.url
            }
          )

        }

      }

      "return a technical error" when {

        "applicant is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[RuntimeException](await(performAction()))
        }

      }
    }

    "handling submit on the recently started trading page" must {

      val date                        = LocalDate.of(2020, 10, 5)
      val recentlyStartedTradingRoute = routes.CompanyDetailsController.recentlyStartedTrading()
      val validJourneyData            = retrievedJourneyDataWithCompanyName.copy(
        ctStatus = Some(
          CTStatusResponse(CTUTR("utr"), date, date, None)
        )
      )

      def performAction(data: (String, String)*): Future[Result] =
        controller.recentlyStartedTradingSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {
        val session = CompanyHECSession(
          companyLoginData,
          validJourneyData,
          CompanyUserAnswers.empty.copy(crn = Some(CRN("crn"))),
          None,
          None,
          List.empty
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

          assertThrows[RuntimeException](await(performAction()))
        }

        "the call to update and next fails" in {
          val answers = CompanyUserAnswers.empty.copy(crn = Some(CRN("crn")))
          val session = CompanyHECSession(
            companyLoginData,
            retrievedJourneyDataWithCompanyName.copy(
              ctStatus = Some(
                CTStatusResponse(CTUTR("utr"), date, date, Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound)))
              )
            ),
            answers,
            None,
            None,
            List.empty
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
          val session = CompanyHECSession(companyLoginData, validJourneyData, answers, None, None, List.empty)

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

      val enterCtutrRoute       = routes.CompanyDetailsController.enterCtutr()
      val enterCtutrSubmitRoute = routes.CompanyDetailsController.enterCtutrSubmit()

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

          assertThrows[RuntimeException](await(performAction()))
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
          assertThrows[RuntimeException](await(performAction()))
        }
      }
    }

    "handling submit on enter ctutr page" must {

      val ctutr1          = "1111111111"
      val ctutr2          = "2222222222"
      val enterCtutrRoute = routes.CompanyDetailsController.enterCtutr()
      val crn             = CRN("crn")

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterCtutrSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {
        val userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
        val session     = Fixtures.companyHECSession(
          companyLoginData,
          retrievedJourneyData = Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
          userAnswers = userAnswers
        )

        def test(errorMessageKey: String, formAnswer: (String, String)*) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(enterCtutrRoute, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formAnswer: _*),
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

        "input CTUTR does not match DES CTUTR and" when {

          "ctutr attempts in session is 0 & is incremented by 1" in {
            val now            = ZonedDateTime.now()
            val crn            = CRN("crn")
            val ggCredId       = session.loginData.ggCredId
            val updatedSession = session.copy(ctutrAnswerAttempts = 1)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockCtutrAttemptsServiceCreateOrIncrementAttempts(crn, ggCredId)(
                Right(CtutrAttempts(crn, ggCredId, 1, now))
              )
              mockStoreSession(updatedSession)(Right(()))
              mockJourneyServiceGetPrevious(enterCtutrRoute, session)(mockPreviousCall)
            }

            checkFormErrorIsDisplayed(
              performAction("enterCtutr" -> "2222222222"),
              messageFromMessageKey("enterCtutr.title"),
              messageFromMessageKey("enterCtutr.error.ctutrsDoNotMatch")
            )
          }

        }
      }

      "return a technical error" when {

        "the applicant type is individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[RuntimeException](await(performAction()))
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
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> "some-utr")))
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
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> "some-utr")))
        }

        "the call to delete ctutr attempts fails" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
            userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          )

          val today                   = LocalDate.now
          val lookbackPeriodStartDate = today.minusYears(2).plusDays(1)
          val lookbackPeriodEndDate   = today.minusYears(1)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(LocalDate.now)
            mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
            mockTaxCheckServiceGetCtStatus(CTUTR(ctutr1), lookbackPeriodStartDate, lookbackPeriodEndDate)(
              Left(Error("fetch CT status failed"))
            )
          }
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)))
        }

        "the call to fetch CT status fails" in {
          val session = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
            userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          )

          val today                   = LocalDate.now
          val lookbackPeriodStartDate = today.minusYears(2).plusDays(1)
          val lookbackPeriodEndDate   = today.minusYears(1)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(LocalDate.now)
            mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
            mockTaxCheckServiceGetCtStatus(CTUTR(ctutr1), lookbackPeriodStartDate, lookbackPeriodEndDate)(
              Left(Error("fetch CT status failed"))
            )
          }
          assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)))
        }

        "the call to update and next fails" when {
          "user answer is valid" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            val session = Fixtures.companyHECSession(
              companyLoginData,
              Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
              userAnswers = answers
            )

            val today                   = LocalDate.now
            val lookbackPeriodStartDate = today.minusYears(2).plusDays(1)
            val lookbackPeriodEndDate   = today.minusYears(1)
            val ctStatusResponse        = CTStatusResponse(CTUTR(ctutr1), today, today, None)

            val updatedAnswers       = answers.copy(ctutr = Some(CTUTR(ctutr1)))
            val updatedRetrievedData = session.retrievedJourneyData.copy(ctStatus = Some(ctStatusResponse))
            val updatedSession       = session.copy(userAnswers = updatedAnswers, retrievedJourneyData = updatedRetrievedData)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTimeProviderToday(LocalDate.now)
              mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
              mockTaxCheckServiceGetCtStatus(CTUTR(ctutr1), lookbackPeriodStartDate, lookbackPeriodEndDate)(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                enterCtutrRoute,
                session,
                updatedSession
              )(
                Left(Error("update and next failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr1)))
          }

          "user answer does not match DES CTUTR & maximum CTUTR attempts has been reached" in {
            val session = Fixtures.companyHECSession(
              companyLoginData,
              Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
              ctutrAnswerAttempts = maxCtutrAttempts,
              userAnswers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                enterCtutrRoute,
                session,
                session
              )(
                Left(Error("update and next failed"))
              )
            }
            assertThrows[RuntimeException](await(performAction("enterCtutr" -> ctutr2)))
          }
        }

      }

      "redirect to the next page" when {

        "user gives a valid answer" when {

          "and the ctutr attempts in session is 0" in {
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
                val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
                val session = Fixtures.companyHECSession(
                  companyLoginData,
                  Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(strippedCtutrAnswer))),
                  answers
                )

                val today                   = LocalDate.now
                val lookbackPeriodStartDate = today.minusYears(2).plusDays(1)
                val lookbackPeriodEndDate   = today.minusYears(1)
                val ctStatusResponse        = CTStatusResponse(CTUTR(strippedCtutrAnswer), today, today, None)

                val updatedAnswers       = answers.copy(ctutr = Some(CTUTR(ctutrAnswer)))
                val updatedRetrievedData = session.retrievedJourneyData.copy(ctStatus = Some(ctStatusResponse))
                val updatedSession       =
                  session.copy(userAnswers = updatedAnswers, retrievedJourneyData = updatedRetrievedData)

                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockTimeProviderToday(LocalDate.now)
                  mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
                  mockTaxCheckServiceGetCtStatus(
                    CTUTR(strippedCtutrAnswer),
                    lookbackPeriodStartDate,
                    lookbackPeriodEndDate
                  )(
                    Right(Some(ctStatusResponse))
                  )
                  mockJourneyServiceUpdateAndNext(
                    enterCtutrRoute,
                    session,
                    updatedSession
                  )(Right(mockNextCall))
                }

                checkIsRedirect(performAction("enterCtutr" -> ctutrAnswer), mockNextCall)
              }
            }
          }

          "and the ctutr attempts in session is non-0, the number of attempts should be reset to 0" in {
            val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
            val session = Fixtures.companyHECSession(
              companyLoginData,
              Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR("1111111111"))),
              ctutrAnswerAttempts = 2,
              userAnswers = answers
            )

            val today                   = LocalDate.now
            val lookbackPeriodStartDate = today.minusYears(2).plusDays(1)
            val lookbackPeriodEndDate   = today.minusYears(1)
            val ctStatusResponse        = CTStatusResponse(CTUTR("1111111111"), today, today, None)

            val updatedAnswers       = answers.copy(ctutr = Some(CTUTR("1111111111")))
            val updatedRetrievedData = session.retrievedJourneyData.copy(ctStatus = Some(ctStatusResponse))
            val updatedSession       =
              session.copy(
                userAnswers = updatedAnswers,
                retrievedJourneyData = updatedRetrievedData,
                ctutrAnswerAttempts = 0
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTimeProviderToday(LocalDate.now)
              mockCtutrAttemptsServiceDelete(crn, session.loginData.ggCredId)(Right(()))
              mockTaxCheckServiceGetCtStatus(
                CTUTR("1111111111"),
                lookbackPeriodStartDate,
                lookbackPeriodEndDate
              )(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                enterCtutrRoute,
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("enterCtutr" -> "1111111111"), mockNextCall)

          }

        }

        "user's answer and DES CTUTR do not match & ctutr attempts in session is one less than the max ctutr attempts, counter is incremented" in {
          val answers        = Fixtures.incompleteCompanyUserAnswers(crn = Some(crn))
          val session        = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
            ctutrAnswerAttempts = maxCtutrAttempts - 1,
            userAnswers = answers
          )
          val updatedSession = session.copy(ctutrAnswerAttempts = maxCtutrAttempts)
          val ggCredId       = session.loginData.ggCredId

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceCreateOrIncrementAttempts(crn, ggCredId)(
              Right(CtutrAttempts(crn, ggCredId, maxCtutrAttempts, ZonedDateTime.now))
            )
            mockStoreSession(updatedSession)(Right(()))
            mockJourneyServiceUpdateAndNext(
              enterCtutrRoute,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("enterCtutr" -> ctutr2), mockNextCall)
        }

        "user's answer and DES CTUTR do not match & maximum number of ctutr attempts has been reached" in {
          val answers = Fixtures.incompleteCompanyUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(
            companyLoginData,
            Fixtures.companyRetrievedJourneyData(desCtutr = Some(CTUTR(ctutr1))),
            ctutrAnswerAttempts = maxCtutrAttempts,
            userAnswers = answers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              enterCtutrRoute,
              session,
              session
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("enterCtutr" -> ctutr2), mockNextCall)
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
          mockJourneyServiceGetPrevious(routes.CompanyDetailsController.dontHaveUtr(), session)(mockPreviousCall)
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
              mockPreviousCall.url
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
          mockJourneyServiceGetPrevious(routes.CompanyDetailsController.ctutrNotMatched(), session)(mockPreviousCall)
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

      "display the page" in {
        val session = Fixtures.companyHECSession()
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("tooManyCtutrAttempts.title"),
          { doc =>
            doc.select("#back").isEmpty shouldBe true
            val link = doc.select(".govuk-body > .govuk-link")
            link.attr("href") shouldBe appConfig.taxCheckGuidanceUrl
          }
        )

      }

    }

  }

}
