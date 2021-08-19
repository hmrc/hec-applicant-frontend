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

import cats.instances.future._
import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.connectors.HECConnector
import uk.gov.hmrc.hecapplicantfrontend.models.ApplicantDetails.IndividualApplicantDetails
import uk.gov.hmrc.hecapplicantfrontend.models.HECTaxCheckData.IndividualHECTaxCheckData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.IndividualRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDetails.IndividualTaxDetails
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceDetails, LicenceExpiryDate, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, Error, HECTaxCheck, HECTaxCheckCode, HECTaxCheckData, Name, TaxSituation}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class TaxCheckServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val mockHECConnector = mock[HECConnector]

  val service = new TaxCheckServiceImpl(mockHECConnector)

  def mockSaveTaxCheck(taxCheckData: HECTaxCheckData)(result: Either[Error, HttpResponse]) =
    (mockHECConnector
      .saveTaxCheck(_: HECTaxCheckData)(_: HeaderCarrier))
      .expects(taxCheckData, *)
      .returning(EitherT.fromEither(result))

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val emptyHeaders = Map.empty[String, Seq[String]]

  "TaxCheckServiceImpl" when {

    "handling requests to save a tax check" must {

      val sautr                   = SAUTR("sautr")
      val email                   = EmailAddress("email")
      val individualRetrievedData = IndividualRetrievedData(
        GGCredId("cred"),
        NINO("nino"),
        Some(sautr),
        Name("first", "last"),
        DateOfBirth(LocalDate.now()),
        Some(email)
      )

      val completeAnswers = CompleteUserAnswers(
        LicenceType.OperatorOfPrivateHireVehicles,
        LicenceExpiryDate(TimeUtils.today().plusDays(1L)),
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToOneYear,
        TaxSituation.SA,
        None
      )

      val individualTaxCheckData = IndividualHECTaxCheckData(
        IndividualApplicantDetails(
          individualRetrievedData.ggCredId,
          individualRetrievedData.name,
          individualRetrievedData.dateOfBirth
        ),
        LicenceDetails(
          completeAnswers.licenceType,
          completeAnswers.licenceExpiryDate,
          completeAnswers.licenceTimeTrading,
          completeAnswers.licenceValidityPeriod
        ),
        IndividualTaxDetails(
          individualRetrievedData.nino,
          Some(sautr)
        )
      )

      val taxCheckCode = HECTaxCheckCode("code")
      val taxCheck     = HECTaxCheck(taxCheckCode, LocalDate.now().plusDays(2L))
      val taxCheckJson = Json.toJson(taxCheck)

      "return an error" when {

        "the http response does not come back with status 201 (created)" in {
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(OK, taxCheckJson, emptyHeaders)))

          val result = service.saveTaxCheck(individualRetrievedData, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no json in the response" in {
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(CREATED, "hi")))

          val result = service.saveTaxCheck(individualRetrievedData, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the json in the response cannot be parsed" in {
          val json = Json.parse("""{ "a" : 1 }""")
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(CREATED, json, emptyHeaders)))

          val result = service.saveTaxCheck(individualRetrievedData, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "the tax check has been saved and the json response can be parsed" in {
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(CREATED, taxCheckJson, emptyHeaders)))

          val result = service.saveTaxCheck(individualRetrievedData, completeAnswers)
          await(result.value) shouldBe Right(taxCheck)
        }

      }

    }

  }

}
