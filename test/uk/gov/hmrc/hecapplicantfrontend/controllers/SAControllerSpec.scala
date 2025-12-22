/*
 * Copyright 2023 HM Revenue & Customs
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

import org.jsoup.nodes.Document
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.*
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.LocalDate
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

  val individualLoginData: IndividualLoginData =
    IndividualLoginData(
      GGCredId(""),
      NINO(""),
      None,
      Name("", ""),
      DateOfBirth(LocalDate.now()),
      None,
      None
    )

  "SAController" when {

    def testLink(doc: Document, url: String) = {
      val link = doc.select(s"a.govuk-link[href=$url]")
      link.size() shouldBe 1
    }

    "handling requests to see the SAUTR not found exit page" must {

      def performAction(): Future[Result] = controller.sautrNotFound(FakeRequest())

      behave like authAndSessionDataBehaviour(() => performAction())

      "display the page" in {

        val session = IndividualHECSession.newSession(individualLoginData)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.SAController.sautrNotFound, session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("sautrNotFound.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            testLink(doc, routes.TaxSituationController.taxSituation.url)
            testLink(doc, appConfig.registerForSaUrl)
            testLink(doc, appConfig.contactHmrcSaUrl)
          }
        )

      }

    }

    "handling requests to see the no return found exit page" must {

      def performAction(): Future[Result] = controller.noReturnFound(FakeRequest())

      behave like authAndSessionDataBehaviour(() => performAction())

      "display the page" in {

        val session = IndividualHECSession.newSession(individualLoginData)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.SAController.noReturnFound, session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("noReturnFound.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url
            testLink(doc, appConfig.contactHmrcSaUrl)
          }
        )

      }

    }

    "handling requests to the SA income statement page" must {

      def performAction(): Future[Result] = controller.saIncomeStatement(FakeRequest())

      behave like authAndSessionDataBehaviour(() => performAction())

      "return an error" when {

        "no relevant income tax year can be found" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement, session)(mockPreviousCall)
          }

          an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
        }

      }

      "display the page" when {

        val relevantIncomeTaxYear = TaxYear(2020)

        def displayPageTest(session: HECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("saIncomeDeclared.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              doc.select(".govuk-hint").text   shouldBe messageFromMessageKey(
                "saIncomeDeclared.hint",
                relevantIncomeTaxYear.startYear.toString,
                (relevantIncomeTaxYear.startYear + 1).toString
              )

              testRadioButtonOptions(
                doc,
                List(messageFromMessageKey("saIncomeDeclared.yes"), messageFromMessageKey("saIncomeDeclared.no")),
                List(None, None)
              )
              val selectedOptions = doc.select(".govuk-radios__input[checked]")

              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.SAController.saIncomeStatementSubmit.url
            }
          )
        }

        "the user has not previously answered the question" in {
          val session = IndividualHECSession
            .newSession(individualLoginData)
            .copy(relevantIncomeTaxYear = Some(relevantIncomeTaxYear))

          displayPageTest(session, None)

        }

        "the user has previously answered the question" in {
          val session =
            Fixtures
              .individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                Fixtures.completeIndividualUserAnswers(
                  LicenceType.DriverOfTaxisAndPrivateHires,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToTwoYears,
                  TaxSituation.PAYE,
                  Some(YesNoAnswer.Yes),
                  Some(EntityType.Individual)
                )
              )
              .copy(relevantIncomeTaxYear = Some(relevantIncomeTaxYear))
          displayPageTest(session, Some("0"))
        }

      }

    }

    "handling submits to the SA income statement page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.saIncomeStatementSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      val relevantIncomeTaxYear = TaxYear(2020)

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session =
          IndividualHECSession.newSession(individualLoginData).copy(relevantIncomeTaxYear = Some(relevantIncomeTaxYear))

        def formErrorTest(data: (String, String)*)(errorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.SAController.saIncomeStatement, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey("saIncomeDeclared.title"),
            messageFromMessageKey(errorMessageKey)
          )
        }

        "nothing is submitted" in {
          formErrorTest()("saIncomeDeclared.error.required")
        }

        "an index is submitted which is too large" in {
          formErrorTest("entityType" -> Int.MaxValue.toString)("saIncomeDeclared.error.invalid")
        }

        "a value is submitted which is not a number" in {
          formErrorTest("entityType" -> "xyz")("saIncomeDeclared.error.invalid")
        }

      }

      "return a technical error" when {

        "no relevant income tax year can be found" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(IndividualHECSession.newSession(individualLoginData))
          }

          an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
        }

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty
          val updatedAnswers = IndividualUserAnswers.empty.copy(saIncomeDeclared = Some(YesNoAnswer.Yes))
          val session        =
            Fixtures
              .individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
              .copy(relevantIncomeTaxYear = Some(relevantIncomeTaxYear))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.SAController.saIncomeStatement, session, updatedSession)(
              Left(Error(new Exception))
            )
          }

          assertThrows[RuntimeException](await(performAction("saIncomeDeclared" -> "0")))
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          def redirectTest(session: HECSession, updatedSession: HECSession, value: String) = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.SAController.saIncomeStatement, session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("saIncomeDeclared" -> value), mockNextCall)
          }
          "the user has not previously completed answering questions" in {
            val answers        = IndividualUserAnswers.empty
            val updatedAnswers = IndividualUserAnswers.empty.copy(saIncomeDeclared = Some(YesNoAnswer.No))
            val session        =
              Fixtures
                .individualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  answers
                )
                .copy(relevantIncomeTaxYear = Some(relevantIncomeTaxYear))
            val updatedSession = session.copy(userAnswers = updatedAnswers)
            redirectTest(session, updatedSession, "1")
          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              Some(YesNoAnswer.Yes)
            )
            val updatedAnswers = IncompleteIndividualUserAnswers
              .fromCompleteAnswers(answers)
              .copy(saIncomeDeclared = Some(YesNoAnswer.No))
            val session        =
              Fixtures
                .individualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  answers
                )
                .copy(relevantIncomeTaxYear = Some(relevantIncomeTaxYear))
            val updatedSession = session.copy(userAnswers = updatedAnswers)
            redirectTest(session, updatedSession, "1")

          }
        }

      }

    }

  }

}
