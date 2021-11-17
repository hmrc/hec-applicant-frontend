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
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.models.CtutrAttempts
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.mongo.cache.DataKey

import scala.concurrent.ExecutionContext.Implicits.global

class CtutrAttemptsStoreImplSpec extends AnyWordSpec with Matchers with MongoSupportSpec with Eventually {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        | ctutr-attempts { store-expiry-time = 30 minutes }
        |""".stripMargin
    )
  )

  val store: CtutrAttemptsStoreImpl = new CtutrAttemptsStoreImpl(mongoComponent, config)

  "CtutrAttemptsStoreImpl" must {

    val crn      = CRN("crn")
    val ggCredId = GGCredId("ggCredId")

    "be able to insert ctutr attempts into mongo, read it back and delete it" in {
      val ctutrAttempts = CtutrAttempts(crn, ggCredId, 1, None)

      // store a ctutr attempts object
      await(store.store(ctutrAttempts).value) shouldBe Right(())

      // check we can get it back
      eventually {
        await(store.get(crn, ggCredId).value) should be(Right(Some(ctutrAttempts)))
      }

      // check that delete returns ok
      eventually {
        await(store.delete(crn, ggCredId).value) should be(Right(()))
      }

      // check that delete actually happened
      eventually {
        await(store.get(crn, ggCredId).value) should be(Right(None))
      }

    }

    "return an error" when {

      "the data in mongo cannot be parsed" in {
        val id = s"${crn.value}-${ggCredId.value}"

        // store a ctutr attempts object
        val create = await {
          store.put(id)(DataKey("hec-ctutr-attempts"), Json.obj("invalid" -> "data"))
        }

        create.id shouldBe id

        // verify that parse fails
        await(store.get(crn, ggCredId).value).isLeft shouldBe true
      }

    }

    "return None if there is no data in mongo" in {
      await(store.get(CRN("some-crn"), GGCredId("some-cred-id")).value) shouldBe Right(None)
    }

  }

}
