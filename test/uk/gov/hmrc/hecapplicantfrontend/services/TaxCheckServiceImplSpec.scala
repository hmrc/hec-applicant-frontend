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
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceDetails, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.{AccountingPeriod, CTStatus, CTStatusResponse, DateOfBirth, EmailAddress, Error, HECTaxCheck, HECTaxCheckCode, HECTaxCheckData, Name, SAStatus, SAStatusResponse, TaxSituation, TaxYear}
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

  def mockGetSAStatus(sautr: SAUTR, taxYear: TaxYear)(result: Either[Error, HttpResponse]) =
    (mockHECConnector
      .getSAStatus(_: SAUTR, _: TaxYear)(_: HeaderCarrier))
      .expects(sautr, taxYear, *)
      .returning(EitherT.fromEither(result))

  def mockGetCTStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(result: Either[Error, HttpResponse]) =
    (mockHECConnector
      .getCTStatus(_: CTUTR, _: LocalDate, _: LocalDate)(_: HeaderCarrier))
      .expects(ctutr, startDate, endDate, *)
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
        Some(email),
        None
      )

      val completeAnswers = CompleteUserAnswers(
        LicenceType.OperatorOfPrivateHireVehicles,
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
          completeAnswers.licenceTimeTrading,
          completeAnswers.licenceValidityPeriod
        ),
        IndividualTaxDetails(
          individualRetrievedData.nino,
          Some(sautr),
          completeAnswers.taxSituation
        )
      )

      val taxCheckCode = HECTaxCheckCode("code")
      val taxCheck     = HECTaxCheck(taxCheckCode, LocalDate.now().plusDays(2L))
      val taxCheckJson = Json.toJson(taxCheck)

      "return an error" when {

        "the http call fails" in {
          mockSaveTaxCheck(individualTaxCheckData)(Left(Error("")))

          val result = service.saveTaxCheck(individualRetrievedData, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

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

    "handling requests to get SA statuses" must {

      val sautr = SAUTR("1234567890")

      val taxYear = TaxYear(2022)

      val saStatusResponse = SAStatusResponse(sautr, taxYear, SAStatus.ReturnFound)

      val saStatusResponseJson = Json.toJson(saStatusResponse)

      "return an error" when {

        "the http call fails" in {
          mockGetSAStatus(sautr, taxYear)(Left(Error("")))

          val result = service.getSAStatus(sautr, taxYear)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-OK (200) response" in {
          mockGetSAStatus(sautr, taxYear)(Right(HttpResponse(ACCEPTED, saStatusResponseJson, emptyHeaders)))

          val result = service.getSAStatus(sautr, taxYear)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no json in the response" in {
          mockGetSAStatus(sautr, taxYear)(Right(HttpResponse(OK, "", emptyHeaders)))

          val result = service.getSAStatus(sautr, taxYear)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the json in the body cannot be parsed" in {
          mockGetSAStatus(sautr, taxYear)(Right(HttpResponse(ACCEPTED, Json.parse("{ }"), emptyHeaders)))

          val result = service.getSAStatus(sautr, taxYear)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "the http call succeeds and the body of the repsonse can be parsed" in {
          mockGetSAStatus(sautr, taxYear)(Right(HttpResponse(OK, saStatusResponseJson, emptyHeaders)))

          val result = service.getSAStatus(sautr, taxYear)
          await(result.value) shouldBe Right(saStatusResponse)
        }

      }

    }

    "handling requests to get CT statuses" must {

      val ctutr = CTUTR("1234567890")

      val startDate = LocalDate.now()
      val endDate   = startDate.plusDays(1L)

      val ctStatusResponse = CTStatusResponse(
        ctutr,
        startDate,
        endDate,
        CTStatus.ReturnFound,
        List(AccountingPeriod("01", LocalDate.now.plusDays(2L), LocalDate.now().plusDays(3L)))
      )

      val ctStatusResponseJson = Json.toJson(ctStatusResponse)

      "return an error" when {

        "the http call fails" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Left(Error("")))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-OK (200) response" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Right(HttpResponse(ACCEPTED, ctStatusResponseJson, emptyHeaders)))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no json in the response" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Right(HttpResponse(OK, "", emptyHeaders)))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the json in the body cannot be parsed" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Right(HttpResponse(ACCEPTED, Json.parse("{ }"), emptyHeaders)))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "the http call succeeds and the body of the repsonse can be parsed" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Right(HttpResponse(OK, ctStatusResponseJson, emptyHeaders)))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe Right(ctStatusResponse)
        }

      }

    }

  }

}
