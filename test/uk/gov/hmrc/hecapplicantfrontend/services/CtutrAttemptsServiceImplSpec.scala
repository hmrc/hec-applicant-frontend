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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.{CtutrAttempts, Error}
import uk.gov.hmrc.hecapplicantfrontend.repos.CtutrAttemptsStore
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider

import java.time.{ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class CtutrAttemptsServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  private val config = Configuration(
    ConfigFactory.parseString(
      """
        | ctutr-attempts {
        |   maximum-attempts = 3
        |   store-expiry-time = 3 hours
        | }
        |""".stripMargin
    )
  )

  val mockCtutrAttemptsStore: CtutrAttemptsStore = mock[CtutrAttemptsStore]

  val mockTimeProvider: TimeProvider = mock[TimeProvider]
  val now: ZonedDateTime             = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))

  private val maxAttempts = 3

  val service: CtutrAttemptsServiceImpl =
    new CtutrAttemptsServiceImpl(mockCtutrAttemptsStore, mockTimeProvider, config)

  private def mockStore(ctutrAttempts: CtutrAttempts)(result: Either[Error, Unit]) =
    (mockCtutrAttemptsStore
      .store(_: CtutrAttempts))
      .expects(ctutrAttempts)
      .returning(EitherT.fromEither(result))

  private def mockGet(crn: CRN, ggCredId: GGCredId)(result: Either[Error, Option[CtutrAttempts]]) =
    (mockCtutrAttemptsStore
      .get(_: CRN, _: GGCredId))
      .expects(crn, ggCredId)
      .returning(EitherT.fromEither(result))

  private def mockTimeProviderNow(d: ZonedDateTime) = (mockTimeProvider.now _).expects().returning(d)

  "CtutrAttemptsServiceImpl" when {

    val crn      = CRN("crn")
    val ggCredId = GGCredId("ggCredId")

    val ctutrAttempts        = CtutrAttempts(crn, ggCredId, 1, None)
    val blockedCtutrAttempts = CtutrAttempts(crn, ggCredId, 1, Some(now))

    "handling requests to update ctutr attempts" must {

      "return an error" when {

        "saving ctutr attempts fails" in {
          mockStore(ctutrAttempts.copy(attempts = 2))(Left(Error("some error")))

          val result = service.updateAttempts(ctutrAttempts)
          await(result.value) shouldBe a[Left[_, _]]
        }
      }

      "return successfully" when {

        "number of attempts is one less than the maximum allowed" in {
          val existingCtutrAttempts = CtutrAttempts(crn, ggCredId, maxAttempts - 1, None)
          val updatedCtutrAttempts  = existingCtutrAttempts.copy(
            attempts = maxAttempts,
            blockedUntil = Some(now.plusHours(3))
          )
          inSequence {
            mockTimeProviderNow(now)
            mockStore(updatedCtutrAttempts)(Right(()))
          }

          val result = service.updateAttempts(existingCtutrAttempts)
          await(result.value) shouldBe Right(updatedCtutrAttempts)
        }

        "number of attempts is more than one less than the maximum allowed" in {
          Seq(0, 1).foreach { attempts =>
            withClue(s"attempts = $attempts") {
              val existingCtutrAttempts = CtutrAttempts(crn, ggCredId, attempts, None)
              val updatedCtutrAttempts  = existingCtutrAttempts.copy(attempts = attempts + 1)

              mockStore(updatedCtutrAttempts)(Right(()))

              val result = service.updateAttempts(existingCtutrAttempts)
              await(result.value) shouldBe Right(updatedCtutrAttempts)
            }
          }
        }

        "existing ctutr attempts is blocked" in {
          val result = service.updateAttempts(blockedCtutrAttempts)
          await(result.value) shouldBe Right(blockedCtutrAttempts)
        }

      }

    }

    "handling requests to get ctutr attempts" must {
      "return default if no data found" in {
        mockGet(crn, ggCredId)(Right(None))

        val result = service.getWithDefault(crn, ggCredId)
        await(result.value) shouldBe Right(CtutrAttempts(crn, ggCredId, 0, None))
      }

      "return fetched data if found" in {
        mockGet(crn, ggCredId)(Right(Some(ctutrAttempts)))

        val result = service.getWithDefault(crn, ggCredId)
        await(result.value) shouldBe Right(ctutrAttempts)
      }

      "throw error when fetch fails" in {
        mockGet(crn, ggCredId)(Left(Error("")))

        val result = service.getWithDefault(crn, ggCredId)
        await(result.value) shouldBe a[Left[_, _]]
      }
    }

  }

}
