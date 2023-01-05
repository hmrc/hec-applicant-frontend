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

package uk.gov.hmrc.hecapplicantfrontend.testonly.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, Error, HECTaxCheckCode}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.HECTaxCheckSource.Digital
import uk.gov.hmrc.hecapplicantfrontend.models.ids.GGCredId
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.testonly.connectors.HECConnector
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.SaveTaxCheckRequest
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class HECServiceImplSpec extends Matchers with AnyWordSpecLike with MockFactory {

  val mockConnector = mock[HECConnector]

  val service = new HECServiceImpl(mockConnector)

  def mockSaveTaxCheck(taxCheckRequest: SaveTaxCheckRequest)(result: Either[Error, HttpResponse]) =
    (mockConnector
      .saveTaxCheck(_: SaveTaxCheckRequest)(_: HeaderCarrier))
      .expects(taxCheckRequest, *)
      .returning(EitherT.fromEither(result))

  "HECServiceImpl" when {

    "handling requests to save a tax check" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val saveTaxCheckRequest = SaveTaxCheckRequest(
        HECTaxCheckCode("code"),
        GGCredId("id"),
        LicenceType.OperatorOfPrivateHireVehicles,
        Right(DateOfBirth(LocalDate.now())),
        LocalDate.now(),
        ZonedDateTime.now(),
        ZonedDateTime.now(),
        isExtracted = true,
        Digital
      )

      "return an error" when {

        "the http call fails" in {
          mockSaveTaxCheck(saveTaxCheckRequest)(Left(Error("")))

          await(service.saveTaxCheck(saveTaxCheckRequest).value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a status which is not 201 (CREATED)" in {
          mockSaveTaxCheck(saveTaxCheckRequest)(Right(HttpResponse(200, "")))

          await(service.saveTaxCheck(saveTaxCheckRequest).value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "the http response comes bck with a status of 201 (CREATED)" in {
          mockSaveTaxCheck(saveTaxCheckRequest)(Right(HttpResponse(201, "")))

          await(service.saveTaxCheck(saveTaxCheckRequest).value) shouldBe Right(())

        }

      }

    }

  }

}
