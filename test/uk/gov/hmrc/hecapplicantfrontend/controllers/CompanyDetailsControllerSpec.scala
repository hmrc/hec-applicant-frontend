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
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{await, defaultAwaitTimeout, status}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CompanyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider    = mock[TimeProvider]
  val mockTaxCheckService = mock[TaxCheckService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[TaxCheckService].toInstance(mockTaxCheckService)
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

  val controller                          = instanceOf[CompanyDetailsController]
  val companyLoginData                    = Fixtures.companyLoginData()
  val retrievedJourneyDataWithCompanyName =
    Fixtures.companyRetrievedJourneyData(companyName = Some(CompanyHouseName("some-company")))

  "CompanyDetailsControllerSpec" when {

    "handling requests to the confirm company details page " must {

      def performAction(): Future[Result] = controller.confirmCompanyDetails(FakeRequest())

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

          val answers = Fixtures.completeUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            companyDetailsConfirmed = Some(YesNoAnswer.Yes)
          )
          val session = Fixtures.companyHECSession(companyLoginData, retrievedJourneyDataWithCompanyName, answers)

          val updatedAnswers = IncompleteUserAnswers
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

      "return internal server error" when {
        "company name is not populated" in {
          val session = Fixtures.companyHECSession()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
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
          Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

      "return an internal server error" when {

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

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
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
            val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
          }

          "the call to update and next fails" in {
            val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
          }
        }

        "user answers with a No" when {
          "the call to update and next fails" in {
            val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

            status(performAction("confirmCompanyName" -> "1")) shouldBe INTERNAL_SERVER_ERROR
          }
        }

      }

      "redirect to the next page" when {

        "user answers with a Yes and all data fetches are successful" when {
          "the enrolment and DES CTUTRs match" in {
            val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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
            val answers     = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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
          val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

          val session = CompanyHECSession(companyLoginData, companyData, UserAnswers.empty, None, None, List.empty)

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

          val answers = CompleteUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            None,
            None,
            None,
            None,
            Some(YesNoAnswer.Yes),
            Some(YesNoAnswer.No),
            None,
            None
          )
          val session = CompanyHECSession(companyLoginData, companyData, answers, None, None, List.empty)

          val updatedAnswers = IncompleteUserAnswers
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

      "return internal server error" when {
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
            UserAnswers.empty,
            None,
            None,
            List.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
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
          Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

      "return an internal server error" when {

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

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "the call to update and next fails" in {
          val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

          status(performAction("chargeableForCT" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "user gives a valid answer" in {
          val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
          val session = Fixtures.companyHECSession(companyLoginData, validJourneyData, answers)

          val updatedAnswers = answers.copy(chargeableForCT = Some(YesNoAnswer.No))
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

    "handling requests to the CT income statement page " must {

      val date                         = LocalDate.of(2020, 10, 5)
      val ctIncomeStatementRoute       = routes.CompanyDetailsController.ctIncomeStatement()
      val ctIncomeStatementSubmitRoute = routes.CompanyDetailsController.ctIncomeStatementSubmit()

      def performAction(): Future[Result] = controller.ctIncomeStatement(FakeRequest())

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

          val answers = Fixtures.completeUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            companyDetailsConfirmed = Some(YesNoAnswer.Yes),
            chargeableForCT = Some(YesNoAnswer.No),
            ctIncomeDeclared = Some(YesNoAnswer.Yes)
          )
          val session = Fixtures.companyHECSession(companyLoginData, companyData, answers)

          val updatedAnswers = IncompleteUserAnswers
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

      "return internal server error" when {
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

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
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
          Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

      "return an internal server error" when {

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

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "the call to update and next fails" in {
          val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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

          status(performAction("ctIncomeDeclared" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "user gives a valid answer" in {
          val answers = Fixtures.incompleteUserAnswers(crn = Some(CRN("crn")))
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
  }

}
