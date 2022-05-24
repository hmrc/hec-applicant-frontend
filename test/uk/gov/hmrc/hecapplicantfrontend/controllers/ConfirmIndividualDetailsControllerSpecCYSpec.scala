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

import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.i18n.Lang
import play.api.inject.bind
import play.api.mvc.{Cookie, Result}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, Name}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ConfirmIndividualDetailsControllerSpecCYSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {
  override implicit val lang: Lang = Lang("cy")
  override def overrideBindings    = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
  )

  val controller = instanceOf[ConfirmIndividualDetailsController]

  override def additionalConfig = super.additionalConfig.withFallback(
    Configuration(
      ConfigFactory.parseString(
        s"""
           | features.welsh-language-support = true
           | play.i18n.langs = ["en", "cy"]
           |""".stripMargin
      )
    )
  )

  "CompanyDetailsControllerSpec" when {

    "handling requests to the confirm individual details page" must {
      def performAction(): Future[Result] =
        controller.confirmIndividualDetails(FakeRequest().withCookies(Cookie("PLAY_LANG", "cy")))
      behave like (authAndSessionDataBehaviour(performAction))

      "display the page when language toggle is on" when {

        "the user is logged in and individual data can be found" in {
          val name        = Name("First", "Last")
          val dateOfBirth = DateOfBirth(LocalDate.of(2000, 12, 3))

          val session = IndividualHECSession.newSession(
            IndividualLoginData(GGCredId(""), NINO(""), None, name, dateOfBirth, None, None)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails,
              session
            )(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmIndividualDetails.title"),
            { doc =>
              val rowValues = doc.select(".govuk-summary-list__value")
              rowValues.get(0).text shouldBe name.firstName
              rowValues.get(1).text shouldBe name.lastName
              rowValues.get(2).text shouldBe "3 Rhagfyr 2000"

              val link = doc.select(".govuk-body > .govuk-link")
              link.attr("href") shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit.url

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit.url

              doc.select("#back").attr("href")             shouldBe mockPreviousCall.url
              doc.select(".hmrc-language-select__list").text should include regex "English"
              doc.select(".hmrc-language-select__list").text should include regex "Cymraeg"
            }
          )

        }

      }

    }
  }
}
