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

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore

import scala.concurrent.Future

class VerifyControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore)
  )

  val controller = instanceOf[VerifyController]

  "VerifyController" when {

    "handling requests to the 'Agents not supported' page" must {

      def performAction(): Future[Result] = controller.verifyNotSupported(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("verifyNotSupported.title"),
          { doc =>
            val links = doc.select(".govuk-body > .govuk-link")

            links.size                   shouldBe 3
            links.get(0).parent().html   shouldBe messageFromMessageKey(
              "verifyNotSupported.p4",
              appConfig.signOutAndSignBackInUrl
            )
            links.get(1).parent().html   shouldBe messageFromMessageKey(
              "verifyNotSupported.p5",
              appConfig.signOutAndSignBackInUrl
            )
            links.get(2).attr("href")    shouldBe appConfig.applicantServiceGuidanceUrl
            links.get(2).parent().text() shouldBe messageFromMessageKey("verifyNotSupported.link")
          }
        )

      }

    }

  }

}
