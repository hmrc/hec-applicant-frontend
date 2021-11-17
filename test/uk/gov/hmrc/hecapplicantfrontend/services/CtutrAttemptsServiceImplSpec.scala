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
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.{CtutrAttempts, Error}
import uk.gov.hmrc.hecapplicantfrontend.repos.CtutrAttemptsStore
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.{ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.language.postfixOps

class CtutrAttemptsServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val mockCtutrAttemptsStore: CtutrAttemptsStore = mock[CtutrAttemptsStore]

  val mockTimeProvider: TimeProvider = mock[TimeProvider]
  val now: ZonedDateTime             = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))

  private val maxAttempts = 3

  val mockAppConfig: AppConfig = new Fixtures.TestConfig(maxAttempts, 3 hours)

  val service: CtutrAttemptsServiceImpl =
    new CtutrAttemptsServiceImpl(mockCtutrAttemptsStore, mockTimeProvider, mockAppConfig)

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

    "handling requests to create or update ctutr attempts" must {

      val crn      = CRN("crn")
      val ggCredId = GGCredId("ggCredId")

      "return an error" when {

        "fetching ctutr attempts fails" in {
          mockGet(crn, ggCredId)(Left(Error("")))

          val result = service.createOrIncrementAttempts(crn, ggCredId)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "saving ctutr attempts fails" in {
          inSequence {
            mockGet(crn, ggCredId)(Right(None))
            mockStore(CtutrAttempts(crn, ggCredId, 1, None))(Left(Error("some error")))
          }

          val result = service.createOrIncrementAttempts(crn, ggCredId)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "no existing ctutr attempts found for CRN & GGCredId" in {
          val ctutrAttempts = CtutrAttempts(crn, ggCredId, 1, None)
          inSequence {
            mockGet(crn, ggCredId)(Right(None))
            mockStore(ctutrAttempts)(Right(()))
          }

          val result = service.createOrIncrementAttempts(crn, ggCredId)
          await(result.value) shouldBe Right(ctutrAttempts)
        }

        "there exists ctutr attempts for CRN & GGCredId" in {
          val existingCtutrAttempts = CtutrAttempts(crn, ggCredId, 1, None)
          val updatedCtutrAttempts  = existingCtutrAttempts.copy(
            attempts = 2,
            lockedUntil = None
          )
          inSequence {
            mockGet(crn, ggCredId)(Right(Some(existingCtutrAttempts)))
            mockStore(updatedCtutrAttempts)(Right(()))
          }

          val result = service.createOrIncrementAttempts(crn, ggCredId)
          await(result.value) shouldBe Right(updatedCtutrAttempts)
        }

        "existing number of attempts is one less than the maximum allowed" in {
          val existingCtutrAttempts = CtutrAttempts(crn, ggCredId, maxAttempts - 1, None)
          val updatedCtutrAttempts  = existingCtutrAttempts.copy(
            attempts = maxAttempts,
            lockedUntil = Some(now.plusHours(3))
          )
          inSequence {
            mockGet(crn, ggCredId)(Right(Some(existingCtutrAttempts)))
            mockTimeProviderNow(now)
            mockStore(updatedCtutrAttempts)(Right(()))
          }

          val result = service.createOrIncrementAttempts(crn, ggCredId)
          await(result.value) shouldBe Right(updatedCtutrAttempts)
        }

      }

    }

  }

}
