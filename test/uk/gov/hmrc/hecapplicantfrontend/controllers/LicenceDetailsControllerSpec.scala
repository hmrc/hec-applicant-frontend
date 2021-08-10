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
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, Error, HECSession, LicenceType, Name, UserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

import java.time.LocalDate
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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

    "handling requests to the licence type page" must {

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
            }
          )

        }

        "the user has previously answered the question" in {
          val session =
            HECSession(individuaRetrievedlData, CompleteUserAnswers(LicenceType.DriverOfTaxisAndPrivateHires))

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

      "handling submits on the licence type page" must {

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
              val answers        = CompleteUserAnswers(LicenceType.DriverOfTaxisAndPrivateHires)
              val updatedAnswers = IncompleteUserAnswers(Some(LicenceType.ScrapMetalMobileCollector))
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

      "handling requests to the licence type exit page" must {

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
    }
  }

}
