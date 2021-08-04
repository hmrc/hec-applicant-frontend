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
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

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

  "LicenceDetailsController" when {

    "handling requests to the licence type page" must {

      def performAction(): Future[Result] = controller.licenceType(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question" in {}

        "the user has previously answered the question" in {}

      }

    }

    "handling submits on the licence type page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTypeSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        "nothing is submitted" in {}

        "an index is submitted which is too large" in {}

        "a value is submitted which is not a number" in {}

      }

      "return an internal server error" when {

        "the call to update and next fails" in {}

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {}

          "the user has previously completed answering questions" in {}
        }

      }

    }

    "handling requests to the licence type exit page" must {

      def performAction(): Future[Result] = controller.licenceTypeExit(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {}

    }
  }

}
