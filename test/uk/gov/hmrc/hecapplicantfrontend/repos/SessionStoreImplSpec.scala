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
import play.api.libs.json.{JsNumber, JsObject}
import play.api.test.Helpers._
import uk.gov.hmrc.cache.model.{Cache, Id}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.CompanyRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId}
import uk.gov.hmrc.http.{HeaderCarrier, SessionId}
import uk.gov.hmrc.mongo.DatabaseUpdate

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SessionStoreImplSpec extends AnyWordSpec with Matchers with MongoSupport with Eventually {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | session-store.expiry-time = 1 day
        |""".stripMargin
    )
  )

  val sessionStore = new SessionStoreImpl(reactiveMongoComponent, config)

  class TestEnvironment {

    val sessionId = SessionId(UUID.randomUUID().toString)

    implicit val hc: HeaderCarrier = HeaderCarrier(sessionId = Some(sessionId))

  }

  "SessionStoreImpl" must {

    val sessionData =
      HECSession(CompanyRetrievedData(GGCredId("id"), Some(CTUTR("utr")), Some(EmailAddress("email"))))

    "be able to insert SessionData into mongo and read it back" in new TestEnvironment {

      await(sessionStore.store(sessionData).value) shouldBe Right(())

      eventually {
        await(sessionStore.get().value) should be(Right(Some(sessionData)))
      }
    }

    "return no SessionData if there is no data in mongo" in new TestEnvironment {
      await(sessionStore.get().value) shouldBe Right(None)
    }

    "return an error" when {

      "the data in mongo cannot be parsed" in new TestEnvironment {
        val invalidData                           = JsObject(Map("journeyStatus" -> JsNumber(1)))
        val create: Future[DatabaseUpdate[Cache]] =
          sessionStore.cacheRepository.createOrUpdate(
            Id(sessionId.value),
            sessionStore.sessionKey,
            invalidData
          )
        await(create).writeResult.inError      shouldBe false
        await(sessionStore.get().value).isLeft shouldBe true
      }

      "there is no session id in the header carrier" in {
        implicit val hc: HeaderCarrier = HeaderCarrier()

        await(sessionStore.store(sessionData).value).isLeft shouldBe true
        await(sessionStore.get().value).isLeft              shouldBe true
      }

    }
  }

}
