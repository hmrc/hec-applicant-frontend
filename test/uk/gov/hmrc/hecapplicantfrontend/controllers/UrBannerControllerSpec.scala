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

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class UrBannerControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore)
  )

  val controller = instanceOf[UrBannerController]

  "AgentsController" when {

    "handling requests to the 'hide banner' endpoint" must {

      def performAction(): Future[Result] = controller.hideBanner(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "throw an error" when {

        "there is an error updating the session" in {
          val session        = Fixtures.individualHECSession(showUserResearchBanner = None)
          val updatedSession = session.copy(showUserResearchBanner = Some(false))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction()))
        }

      }

      "return a HTTP 200 (ok)" when {

        "the session data does not need to be updated" in {
          val session = Fixtures.companyHECSession(showUserResearchBanner = Some(false))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe OK
        }

        "the session updated is successful updated" in {
          val session        = Fixtures.companyHECSession(showUserResearchBanner = Some(true))
          val updatedSession = session.copy(showUserResearchBanner = Some(false))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          status(performAction()) shouldBe OK
        }

      }

    }

  }

}
