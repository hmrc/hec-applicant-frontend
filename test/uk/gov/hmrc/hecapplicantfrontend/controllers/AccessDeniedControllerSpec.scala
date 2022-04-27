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
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector

import scala.concurrent.Future

class AccessDeniedControllerSpec extends ControllerSpec with AuthSupport {

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector)
    )

  val controller = instanceOf[AccessDeniedController]

  "AccessDeniedController" when {

    "handling requests to display the ScotNI private beta access denied page" must {

      def performAction(): Future[Result] = controller.scotNIPrivateBetaAccessDenied(FakeRequest())

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("accessDenied.title")
        )

      }

    }

  }
}
