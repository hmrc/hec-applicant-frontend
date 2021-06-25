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

import play.api.http.Status.OK
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedUserData.CompanyRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.GGCredId
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class DummyControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore)
  )

  val controller = instanceOf[DummyController]

  "DummyController" when {

    "handling requests to the dummy page" must {

      def performAction(): Future[Result] = controller.dummy(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "display the page" when {

        "the user is logged in and session data can be found" in {
          val session = HECSession(CompanyRetrievedData(GGCredId(""), None, None))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe OK
        }

      }

    }

  }

}
