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

package uk.gov.hmrc.hecapplicantfrontend.repos

import com.typesafe.config.ConfigFactory
import org.scalatest.concurrent.Eventually
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.mvc.Request
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.CompanyLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, HECSession}
import uk.gov.hmrc.http.SessionId

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class SessionStoreImplSpec extends AnyWordSpec with Matchers with MongoSupportSpec with Eventually {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | session-store.expiry-time = 1 day
        |""".stripMargin
    )
  )

  val sessionStore = new SessionStoreImpl(mongoComponent, config)

  class TestEnvironment(sessionData: HECSession) {

    val sessionId                    = SessionId(UUID.randomUUID().toString)
    val fakeRequest                  = AuthenticatedRequest(FakeRequest().withSession(("sessionId", sessionId.toString)))
    implicit val request: Request[_] =
      RequestWithSessionData(fakeRequest, sessionData)
  }

  "SessionStoreImpl" must {

    val sessionData =
      CompanyHECSession.newSession(
        CompanyLoginData(
          GGCredId("id"),
          Some(CTUTR("utr")),
          Some(EmailAddress("email"))
        )
      )

    "be able to insert SessionData into mongo and read it back" in new TestEnvironment(sessionData) {

      await(sessionStore.store(sessionData).value) shouldBe Right(())

      eventually {
        await(sessionStore.get().value) should be(Right(Some(sessionData)))
      }
    }

    "return no SessionData if there is no data in mongo" in new TestEnvironment(sessionData) {
      await(sessionStore.get().value) shouldBe Right(None)
    }

  }

}
