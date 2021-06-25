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

import cats.data.EitherT
import cats.instances.future._
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.models.iv.IvErrorStatus
import uk.gov.hmrc.hecapplicantfrontend.services.IvService
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class IvFailureControllerSpec extends ControllerSpec with AuthSupport {

  val mockIvService = mock[IvService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[IvService].toInstance(mockIvService),
      bind[AuthConnector].toInstance(mockAuthConnector)
    )

  def mockGetFailedJourneyStatus(journeyId: UUID)(result: Either[Error, IvErrorStatus]) =
    (mockIvService
      .getFailedJourneyStatus(_: UUID)(_: HeaderCarrier))
      .expects(journeyId, *)
      .returning(EitherT.fromEither(result))

  lazy val controller = instanceOf[IvFailureController]

  "IvFailureController" when {

    "handling IV failure callbacks" must {

      def performAction(journeyId: UUID): Future[Result] =
        controller.ivFailure(journeyId)(FakeRequest())

      "return an InternalServerError" when {

        "there is an error getting the failed journey status" in {
          val journeyId = UUID.randomUUID()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetFailedJourneyStatus(journeyId)(Left(Error("")))
          }

          status(performAction(journeyId)) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "return a dummy response when the failed journey status is retrieved" in {
        val journeyId = UUID.randomUUID()

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetFailedJourneyStatus(journeyId)(Right(IvErrorStatus.Timeout))
        }

        val result = performAction(journeyId)
        status(result)          shouldBe OK
        contentAsString(result) shouldBe (s"Got IV error status ${IvErrorStatus.Timeout}")
      }

    }

  }

}
