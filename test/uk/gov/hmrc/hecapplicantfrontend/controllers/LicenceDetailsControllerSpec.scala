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

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceValidityPeriod.UpToOneYear
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class LicenceDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
  )

  val controller = instanceOf[LicenceDetailsController]

  val individuaRetrievedlData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None)

  "LicenceDetailsController" when {

    "handling requests to the licence type page" ignore {

      def performAction(): Future[Result] = controller.licenceType(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceType.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              doc.select(".govuk-body > .govuk-link").attr("href") shouldBe routes.LicenceDetailsController
                .licenceTypeExit()
                .url

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTypeSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {
          val session =
            HECSession(
              individuaRetrievedlData,
              CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToTwoYears
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceType.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "0"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTypeSubmit().url
            }
          )
        }

        "the back location is the start endpoint" in {
          val session = HECSession(companyRetrievedData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(
              routes.StartController.start()
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceType.title"),
            _.select("#back").isEmpty shouldBe true
          )

        }

      }

    }

    "handling submits on the licence type page" ignore {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTypeSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("licenceType" -> Int.MaxValue.toString),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("licenceType" -> "xyz"),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.error.invalid")
          )
        }

      }

      "return an internal server error" when {

        "the call to update and next fails" in {
          val answers        = UserAnswers.empty
          val updatedAnswers = UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
          val session        = HECSession(individuaRetrievedlData, answers)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType(), session, updatedSession)(
              Left(Error(new Exception))
            )
          }

          status(performAction("licenceType" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = UserAnswers.empty
            val updatedAnswers = UserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
            val session        = HECSession(individuaRetrievedlData, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType(), session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceType" -> "1"), mockNextCall)
          }

          "the user has previously completed answering questions" in {
            val answers        = CompleteUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val updatedAnswers = IncompleteUserAnswers
              .fromCompleteAnswers(answers)
              .copy(licenceType = Some(LicenceType.ScrapMetalMobileCollector))
            val session        = HECSession(individuaRetrievedlData, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType(), session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceType" -> "2"), mockNextCall)
          }
        }

      }

    }

    "handling requests to the licence type exit page" ignore {

      def performAction(): Future[Result] = controller.licenceTypeExit(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {
        val session = HECSession(individuaRetrievedlData, UserAnswers.empty)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTypeExit(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceTypeExit.title"),
          doc => doc.select("#back").attr("href") shouldBe mockPreviousCall.url
        )

      }

    }

    "handling requests to licence expiry page" ignore {

      def performAction(): Future[Result] =
        controller.expiryDate(FakeRequest())

      behave like authAndSessionDataBehaviour(() => performAction())

      "display the page" when {

        "a previous answer to the question is not found in session" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDate(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceExpiryDate.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              doc.select("#licenceExpiryDate-day").attr("value")   shouldBe ""
              doc.select("#licenceExpiryDate-month").attr("value") shouldBe ""
              doc.select("#licenceExpiryDate-year").attr("value")  shouldBe ""

              doc.select(".govuk-body > .govuk-link").attr("href") shouldBe routes.LicenceDetailsController
                .expiryDateExit()
                .url

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.expiryDateSubmit().url
            }
          )

        }

        "a previous answer to the question is found in session" in {
          val date    = TimeUtils.today()
          val session =
            HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(licenceExpiryDate = Some(LicenceExpiryDate(date)))
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDate(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceExpiryDate.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              doc.select("#licenceExpiryDate-day").attr("value")   shouldBe date.getDayOfMonth.toString
              doc.select("#licenceExpiryDate-month").attr("value") shouldBe date.getMonthValue.toString
              doc.select("#licenceExpiryDate-year").attr("value")  shouldBe date.getYear.toString

              doc.select(".govuk-body > .govuk-link").attr("href") shouldBe routes.LicenceDetailsController
                .expiryDateExit()
                .url

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.expiryDateSubmit().url
            }
          )
        }

      }

    }

    "handling submits on the licence Expiry page" ignore {

      def performAction(data: (String, String)*): Future[Result] =
        controller.expiryDateSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      def formData(date: LocalDate): List[(String, String)] = List(
        "licenceExpiryDate-day"   -> date.getDayOfMonth.toString,
        "licenceExpiryDate-month" -> date.getMonthValue.toString,
        "licenceExpiryDate-year"  -> date.getYear.toString
      )

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDate(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceExpiryDate.title"),
            messageFromMessageKey("licenceExpiryDate.error.required")
          )
        }

        "the date entered is invalid" in {
          DateErrorScenarios
            .dateErrorScenarios("licenceExpiryDate")
            .foreach { scenario =>
              withClue(s"For date error scenario $scenario: ") {
                val data = List(
                  "licenceExpiryDate-day"   -> scenario.dayInput,
                  "licenceExpiryDate-month" -> scenario.monthInput,
                  "licenceExpiryDate-year"  -> scenario.yearInput
                ).collect { case (key, Some(value)) => key -> value }
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDate(), session)(
                    mockPreviousCall
                  )
                }

                checkFormErrorIsDisplayed(
                  performAction(data: _*),
                  messageFromMessageKey("licenceExpiryDate.title"),
                  messageFromMessageKey(scenario.expectedErrorMessageKey)
                )
              }
            }
        }

        "date entered is more than 6 years in the future" in {
          val cutoffDate = TimeUtils.today().plusYears(6L)
          val date       = cutoffDate.plusDays(1L)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDate(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formData(date): _*),
            messageFromMessageKey("licenceExpiryDate.title"),
            messageFromMessageKey("licenceExpiryDate.error.tooFarInFuture", TimeUtils.govDisplayFormat(cutoffDate))
          )
        }

        "date entered is more than 2 years in the past" in {
          val cutoffDate = TimeUtils.today().minusYears(2L)
          val date       = cutoffDate.minusDays(1L)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDate(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(formData(date): _*),
            messageFromMessageKey("licenceExpiryDate.title"),
            messageFromMessageKey("licenceExpiryDate.error.tooFarInPast", TimeUtils.govDisplayFormat(cutoffDate))
          )
        }

      }

      "return an InternalServerError" when {

        "there is an error updating and getting the next endpoint" in {
          val date    = TimeUtils.today().plusYears(6L)
          val answers = UserAnswers.empty
          val session = HECSession(individuaRetrievedlData, answers)

          val updatedAnswers = answers.copy(licenceExpiryDate = Some(LicenceExpiryDate(date)))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.expiryDate(), session, updatedSession)(
              Left(Error(""))
            )
          }

          status(performAction(formData(date): _*)) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "a valid expiry date is submitted" in {
          val date    = TimeUtils.today().minusYears(2L)
          val answers = UserAnswers.empty
          val session = HECSession(individuaRetrievedlData, answers)

          val updatedAnswers = answers.copy(licenceExpiryDate = Some(LicenceExpiryDate(date)))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.expiryDate(), session, updatedSession)(
              Right(mockNextCall)
            )
          }

          checkIsRedirect(performAction(formData(date): _*), mockNextCall)
        }

      }

    }

    "handling requests to the licence expiry date exit page" ignore {

      def performAction(): Future[Result] = controller.expiryDateExit(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {
        val session = HECSession(individuaRetrievedlData, UserAnswers.empty)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.expiryDateExit(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceExpiryDateExit.title"),
          doc => doc.select("#back").attr("href") shouldBe mockPreviousCall.url
        )

      }

    }

    "handling requests to the licence time trading  page" ignore {

      def performAction(): Future[Result] = controller.licenceTimeTrading(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTimeTradingSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {
          val session =
            HECSession(
              individuaRetrievedlData,
              CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
                LicenceTimeTrading.TwoToFourYears,
                LicenceValidityPeriod.UpToThreeYears
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "1"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTimeTradingSubmit().url
            }
          )
        }

      }

    }

    "handling submits on the licence time trading page" ignore {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTimeTradingSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceTimeTrading" -> Int.MaxValue.toString),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceTimeTrading" -> "xyz"),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.error.invalid")
          )
        }

      }

      "return an internal server error" when {

        "the call to update and next fails" in {
          val answers        = UserAnswers.empty
          val updatedAnswers = UserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears))
          val session        = HECSession(individuaRetrievedlData, answers)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.LicenceDetailsController.licenceTimeTrading(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }

          status(performAction("licenceTimeTrading" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = UserAnswers.empty
            val updatedAnswers = UserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.FourToEightYears))
            val session        = HECSession(individuaRetrievedlData, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceTimeTrading" -> "2"), mockNextCall)
          }

          "the user has previously completed answering questions" in {
            val answers        = CompleteUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToFiveYears
            )
            val updatedAnswers = IncompleteUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceExpiryDate(TimeUtils.today().minusDays(10L))),
              Some(LicenceTimeTrading.EightYearsOrMore),
              Some(LicenceValidityPeriod.UpToFiveYears)
            )
            val session        = HECSession(individuaRetrievedlData, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceTimeTrading" -> "3"), mockNextCall)
          }
        }

      }

    }

    "handling requests to the licence validity period page" ignore {

      def performAction(): Future[Result] = controller.recentLicenceLength(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "return an InternalServerError" when {

        "a licence type cannot be found in session" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "display the page" when {

        "the user has selected a licence type of 'operator of private hire vehicles'" in {
          val session = HECSession(
            individuaRetrievedlData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceValidityPeriod.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val options = doc.select(".govuk-radios__item")
              options.size() shouldBe 5

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.recentLicenceLengthSubmit().url
            }
          )

        }

        "the user has selected a licence type which isn't 'Operator of Private Hire Vehicles'" in {
          List(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceType.ScrapMetalMobileCollector,
            LicenceType.ScrapMetalDealerSite
          ).foreach { licenceType =>
            withClue(s"For licence type $licenceType: ") {
              val session =
                HECSession(
                  individuaRetrievedlData,
                  CompleteUserAnswers(
                    licenceType,
                    LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
                    LicenceTimeTrading.TwoToFourYears,
                    LicenceValidityPeriod.UpToThreeYears
                  )
                )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), session)(
                  mockPreviousCall
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("licenceValidityPeriod.title"),
                { doc =>
                  doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                  val options = doc.select(".govuk-radios__item")
                  options.size() shouldBe 3

                  val selectedOptions = doc.select(".govuk-radios__input[checked]")
                  selectedOptions.attr("value") shouldBe "2"

                  val form = doc.select("form")
                  form
                    .attr("action") shouldBe routes.LicenceDetailsController.recentLicenceLengthSubmit().url
                }
              )
            }
          }

        }

      }

    }

    "handling submits on the licence time validity period page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.recentLicenceLengthSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "return an InternalServerError" when {

        "a licence type cannot be found in session" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "the call to update and next fails" in {
          val answers        = UserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
          val updatedAnswers = UserAnswers.empty
            .copy(licenceType = Some(DriverOfTaxisAndPrivateHires), licenceValidityPeriod = Some(UpToOneYear))
          val session        = HECSession(individuaRetrievedlData, answers)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.LicenceDetailsController.recentLicenceLength(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }

          status(performAction("licenceValidityPeriod" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

        "redirect to the next page" when {

          "valid data is submitted and" when {

            "the user has not previously completed answering questions" when {
              val answers        = UserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
              val updatedAnswers = answers.copy(licenceValidityPeriod = Some(UpToOneYear))
              val session        = HECSession(individuaRetrievedlData, answers)
              val updatedSession = session.copy(userAnswers = updatedAnswers)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceUpdateAndNext(
                  routes.LicenceDetailsController.recentLicenceLength(),
                  session,
                  updatedSession
                )(
                  Right(mockNextCall)
                )
              }

              checkIsRedirect(performAction("licenceValidityPeriod" -> "0"), mockNextCall)

            }

            "the user has previously completed answering questions" in {
              val answers        = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToThreeYears
              )
              val updatedAnswers = IncompleteUserAnswers(
                Some(LicenceType.DriverOfTaxisAndPrivateHires),
                Some(LicenceExpiryDate(TimeUtils.today().minusDays(10L))),
                Some(LicenceTimeTrading.ZeroToTwoYears),
                Some(LicenceValidityPeriod.UpToFiveYears)
              )
              val session        = HECSession(individuaRetrievedlData, answers)
              val updatedSession = session.copy(userAnswers = updatedAnswers)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceUpdateAndNext(
                  routes.LicenceDetailsController.recentLicenceLength(),
                  session,
                  updatedSession
                )(
                  Right(mockNextCall)
                )
              }

              checkIsRedirect(performAction("licenceValidityPeriod" -> "5"), mockNextCall)
            }
          }

        }

      }

      "show a form error" when {

        val answers        = UserAnswers.empty
        val updatedAnswers = UserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val session        = HECSession(individuaRetrievedlData, answers)
        val updatedSession = session.copy(userAnswers = updatedAnswers)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceValidityPeriod" -> Int.MaxValue.toString),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceValidityPeriod" -> "xyz"),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.error.invalid")
          )
        }

      }

    }

  }

}
