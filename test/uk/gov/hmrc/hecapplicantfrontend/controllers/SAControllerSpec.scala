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

import java.time.LocalDate

import org.jsoup.nodes.Document
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.IndividualRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SAControllerSpec
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

  private val controller = instanceOf[SAController]

  val individuaRetrievedlData: IndividualRetrievedData =
    IndividualRetrievedData(
      GGCredId(""),
      NINO(""),
      None,
      Name("", ""),
      DateOfBirth(LocalDate.now()),
      None,
      None,
      List.empty
    )

  "SAController" when {

    def testLink(doc: Document, url: String) = {
      val link = doc.select(s"a.govuk-link[href=$url]")
      link.size() shouldBe 1
    }

    "handling requests to see the SAUTR not found exit page" must {

      def performAction(): Future[Result] = controller.sautrNotFound(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {

        val session = HECSession(
          individuaRetrievedlData,
          UserAnswers.empty,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.SAController.sautrNotFound(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("sautrNotFound.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            testLink(doc, routes.TaxSituationController.taxSituation().url)
            testLink(doc, appConfig.registerForSaUrl)
            testLink(doc, appConfig.contactHmrcSa)
          }
        )

      }

    }

    "handling requests to see the no return found exit page" must {

      def performAction(): Future[Result] = controller.noReturnFound(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {

        val session = HECSession(
          individuaRetrievedlData,
          UserAnswers.empty,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.SAController.noReturnFound(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("noReturnFound.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url
            testLink(doc, appConfig.contactHmrcSa)
          }
        )

      }

    }

    "handling requests to the SA income statement page" must {

      def performAction(): Future[Result] = controller.saIncomeStatement(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("saIncomeDeclared.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.SAController.saIncomeStatementSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {
          val session =
            HECSession(
              individuaRetrievedlData,
              CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToTwoYears,
                Some(TaxSituation.PAYE),
                Some(YesNoAnswer.Yes),
                Some(EntityType.Individual),
                None,
                None
              ),
              None
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("saIncomeDeclared.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "0"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.SAController.saIncomeStatementSubmit().url
            }
          )
        }

      }

    }

    "handling submits to the SA income statement page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.saIncomeStatementSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = HECSession(individuaRetrievedlData, UserAnswers.empty, None)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("saIncomeDeclared.title"),
            messageFromMessageKey("saIncomeDeclared.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("entityType" -> Int.MaxValue.toString),
            messageFromMessageKey("saIncomeDeclared.title"),
            messageFromMessageKey("saIncomeDeclared.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("entityType" -> "xyz"),
            messageFromMessageKey("saIncomeDeclared.title"),
            messageFromMessageKey("saIncomeDeclared.error.invalid")
          )
        }

      }

      "return an internal server error" when {

        "the call to update and next fails" in {
          val answers        = UserAnswers.empty
          val updatedAnswers = UserAnswers.empty.copy(saIncomeDeclared = Some(YesNoAnswer.Yes))
          val session        = HECSession(individuaRetrievedlData, answers, None)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.SAController.saIncomeStatement(), session, updatedSession)(
              Left(Error(new Exception))
            )
          }

          status(performAction("saIncomeDeclared" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = UserAnswers.empty
            val updatedAnswers = UserAnswers.empty.copy(saIncomeDeclared = Some(YesNoAnswer.No))
            val session        = HECSession(individuaRetrievedlData, answers, None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.SAController.saIncomeStatement(), session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("saIncomeDeclared" -> "1"), mockNextCall)
          }

          "the user has previously completed answering questions" in {
            val answers        = CompleteUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              Some(TaxSituation.SA),
              Some(YesNoAnswer.Yes),
              None,
              None,
              None
            )
            val updatedAnswers = IncompleteUserAnswers
              .fromCompleteAnswers(answers)
              .copy(saIncomeDeclared = Some(YesNoAnswer.No))
            val session        = HECSession(individuaRetrievedlData, answers, None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.SAController.saIncomeStatement(), session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("saIncomeDeclared" -> "1"), mockNextCall)
          }
        }

      }

    }

  }

}
