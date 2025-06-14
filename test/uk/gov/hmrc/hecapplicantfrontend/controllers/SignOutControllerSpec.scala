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

import play.api.mvc.Session
import play.api.test.FakeRequest
import play.api.test.Helpers._

class SignOutControllerSpec extends ControllerSpec {

  val controller = instanceOf[SignOutController]

  "SignOutController" when {

    "handling requests to the 'sign out from timeout' page" must {

      "display the page and clear the http session" in {
        val result = controller.signOutFromTimeout(FakeRequest().withSession("key" -> "value"))

        checkPageIsDisplayed(
          result,
          messageFromMessageKey("timedOut.title"),
          { doc =>
            val link = doc.select("a.govuk-button")
            link.attr("href") shouldBe routes.StartController.start.url
          }
        )

        session(result) shouldBe Session()
      }

    }

    "handling requests to the 'exit survey' endpoint" must {

      "clear the http session and redirect to the exit survey" in {
        val result = controller.exitSurvey(FakeRequest().withSession("key" -> "value"))

        checkIsRedirect(result, appConfig.exitSurveySignOutUrl)
        session(result) shouldBe Session()
      }

    }

  }

}
