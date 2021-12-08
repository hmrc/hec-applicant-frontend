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
import cats.implicits.catsSyntaxOptionId
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.connectors.HECConnector
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.ApplicantDetails.IndividualApplicantDetails
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.HECTaxCheckData.IndividualHECTaxCheckData
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.TaxDetails.IndividualTaxDetails
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.{HECTaxCheckData, HECTaxCheckSource}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.{CTAccountingPeriod, CTStatus, CTStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.{SAStatus, SAStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceDetails, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, Error, HECTaxCheck, HECTaxCheckCode, Name, TaxCheckListItem, TaxSituation, TaxYear, YesNoAnswer}
import uk.gov.hmrc.hecapplicantfrontend.services.TaxCheckService._
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class TaxCheckServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val mockHECConnector: HECConnector  = mock[HECConnector]
  val zonedDateTimeNow: ZonedDateTime = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))
  val service: TaxCheckServiceImpl    = new TaxCheckServiceImpl(mockHECConnector)

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

  def mockGetCtutr(crn: CRN)(result: Either[Error, HttpResponse]) =
    (mockHECConnector
      .getCtutr(_: CRN)(_: HeaderCarrier))
      .expects(crn, *)
      .returning(EitherT.fromEither(result))

  def mockGetUnexpiredTaxChecks(result: Either[Error, HttpResponse]) =
    (mockHECConnector
      .getUnexpiredTaxCheckCodes()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT.fromEither(result))

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val incomeTaxYear = TaxYear(2021)

  val emptyHeaders = Map.empty[String, Seq[String]]

  "TaxCheckServiceImpl" when {

    "handling requests to save a tax check" must {

      val sautr               = SAUTR("sautr")
      val email               = EmailAddress("email")
      val individualLoginData = IndividualLoginData(
        GGCredId("cred"),
        NINO("nino"),
        Some(sautr),
        Name("first", "last"),
        DateOfBirth(LocalDate.now()),
        Some(email)
      )

      val completeAnswers = Fixtures.completeIndividualUserAnswers(
        LicenceType.OperatorOfPrivateHireVehicles,
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToOneYear,
        TaxSituation.SA,
        Some(YesNoAnswer.Yes)
      )

      val retrievedJourneyData = IndividualRetrievedJourneyData(None)

      val individualTaxCheckData = IndividualHECTaxCheckData(
        IndividualApplicantDetails(
          individualLoginData.ggCredId,
          individualLoginData.name,
          individualLoginData.dateOfBirth
        ),
        LicenceDetails(
          completeAnswers.licenceType,
          completeAnswers.licenceTimeTrading,
          completeAnswers.licenceValidityPeriod
        ),
        IndividualTaxDetails(
          individualLoginData.nino,
          Some(sautr),
          completeAnswers.taxSituation,
          completeAnswers.saIncomeDeclared,
          None,
          TaxYear(2021)
        ),
        zonedDateTimeNow,
        HECTaxCheckSource.Digital
      )

      val taxCheckCode = HECTaxCheckCode("code")
      val taxCheck     = HECTaxCheck(taxCheckCode, LocalDate.now().plusDays(2L))
      val taxCheckJson = Json.toJson(taxCheck)

      val individualSession  = Fixtures.individualHECSession(
        individualLoginData,
        retrievedJourneyData,
        completeAnswers,
        None,
        Some(zonedDateTimeNow),
        List.empty,
        incomeTaxYear.some
      )
      val individualSession2 = Fixtures.individualHECSession(
        individualLoginData,
        retrievedJourneyData,
        completeAnswers,
        relevantIncomeTaxYear = incomeTaxYear.some
      )

      "return an error" when {

        "the http call fails" in {
          mockSaveTaxCheck(individualTaxCheckData)(Left(Error("")))

          val result = service.saveTaxCheck(individualSession, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response does not come back with status 201 (created)" in {
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(OK, taxCheckJson, emptyHeaders)))

          val result = service.saveTaxCheck(individualSession, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no json in the response" in {
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(CREATED, "hi")))

          val result = service.saveTaxCheck(individualSession, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the json in the response cannot be parsed" in {
          val json = Json.parse("""{ "a" : 1 }""")
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(CREATED, json, emptyHeaders)))

          val result = service.saveTaxCheck(individualSession, completeAnswers)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no taxCheckStartDateTime in the session" in {

          assertThrows[RuntimeException](await(service.saveTaxCheck(individualSession2, completeAnswers).value))

        }

      }

      "return successfully" when {

        "the tax check has been saved and the json response can be parsed" in {
          mockSaveTaxCheck(individualTaxCheckData)(Right(HttpResponse(CREATED, taxCheckJson, emptyHeaders)))

          val result = service.saveTaxCheck(individualSession, completeAnswers)
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

        "the http call succeeds and the body of the response can be parsed" in {
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
        Some(CTAccountingPeriod(LocalDate.now.plusDays(2L), LocalDate.now().plusDays(3L), CTStatus.ReturnFound))
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

        "the http call succeeds and the body of the response can be parsed" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Right(HttpResponse(OK, ctStatusResponseJson, emptyHeaders)))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe Right(Some(ctStatusResponse))
        }

        "the http call returns a 404 not found status" in {
          mockGetCTStatus(ctutr, startDate, endDate)(Right(HttpResponse(NOT_FOUND, Json.obj(), emptyHeaders)))

          val result = service.getCTStatus(ctutr, startDate, endDate)
          await(result.value) shouldBe Right(None)
        }

      }

    }

    "handling requests to get CTUTR from CRN" must {

      val crn = CRN("AA12345")

      val response = CTUTRFromCRNResponse(CTUTR("111111111"))

      val responseJson = Json.toJson(response)

      "return an error" when {

        "the http call fails" in {
          mockGetCtutr(crn)(Left(Error("")))

          val result = service.getCtutr(crn)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-OK (200) response" in {
          mockGetCtutr(crn)(Right(HttpResponse(ACCEPTED, responseJson, emptyHeaders)))

          val result = service.getCtutr(crn)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no json in the response" in {
          mockGetCtutr(crn)(Right(HttpResponse(OK, "", emptyHeaders)))

          val result = service.getCtutr(crn)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the json in the body cannot be parsed" in {
          mockGetCtutr(crn)(Right(HttpResponse(ACCEPTED, Json.parse("{ }"), emptyHeaders)))

          val result = service.getCtutr(crn)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "the http call succeeds and the body of the response can be parsed" in {
          mockGetCtutr(crn)(Right(HttpResponse(OK, responseJson, emptyHeaders)))

          val result = service.getCtutr(crn)
          await(result.value) shouldBe Right(Some(response.ctutr))
        }

        "the http call returns a 404 not found status" in {
          mockGetCtutr(crn)(Right(HttpResponse(NOT_FOUND, Json.obj(), emptyHeaders)))

          val result = service.getCtutr(crn)
          await(result.value) shouldBe Right(None)
        }

      }

    }

    "handling requests to get unexpired tax checks" must {

      val response = List(
        TaxCheckListItem(
          licenceType = LicenceType.ScrapMetalMobileCollector,
          taxCheckCode = HECTaxCheckCode("some-code"),
          expiresAfter = LocalDate.now(),
          createDate = ZonedDateTime.now()
        )
      )

      val responseJson = Json.toJson(response)

      "return an error" when {

        "the http call fails" in {
          mockGetUnexpiredTaxChecks(Left(Error("")))

          val result = service.getUnexpiredTaxCheckCodes()
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-OK (200) response" in {
          mockGetUnexpiredTaxChecks(Right(HttpResponse(ACCEPTED, responseJson, emptyHeaders)))

          val result = service.getUnexpiredTaxCheckCodes()
          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is no json in the response" in {
          mockGetUnexpiredTaxChecks(Right(HttpResponse(OK, "", emptyHeaders)))

          val result = service.getUnexpiredTaxCheckCodes()
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the json in the body cannot be parsed" in {
          mockGetUnexpiredTaxChecks(Right(HttpResponse(ACCEPTED, Json.parse("{ }"), emptyHeaders)))

          val result = service.getUnexpiredTaxCheckCodes()
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return successfully" when {

        "the http call succeeds and the body of the response can be parsed" in {
          mockGetUnexpiredTaxChecks(Right(HttpResponse(OK, responseJson, emptyHeaders)))

          val result = service.getUnexpiredTaxCheckCodes()
          await(result.value) shouldBe Right(response)
        }

      }

    }

  }

}
