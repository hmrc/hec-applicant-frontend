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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.IndividualRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
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

  val individuaRetrievedlData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)

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

  }

}
