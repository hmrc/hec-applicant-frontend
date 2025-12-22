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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import cats.instances.future.*
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.test.Helpers.*
import uk.gov.hmrc.hecapplicantfrontend.connectors.CompanyDetailsConnector
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyHouseDetails, CompanyHouseName, Error}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CompanyDetailsServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val mockCompanyDetailsConnector = mock[CompanyDetailsConnector]

  def mockFindComapny(companyNumber: CRN)(response: Either[Error, HttpResponse]) =
    (mockCompanyDetailsConnector
      .findCompany(_: CRN)(_: HeaderCarrier))
      .expects(companyNumber, *)
      .returning(EitherT.fromEither[Future](response))

  val companyDetailsService      = new CompanyDetailsServiceImpl(mockCompanyDetailsConnector)
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "CompanyDetailsServiceImpl" when {

    "handling requests to get citizen details" must {

      val responseHeaders = Map.empty[String, Seq[String]]

      "return an error" when {

        def testIsError(mockAction: CRN => Unit): Unit = {
          val companyNumber = CRN("1234567")

          mockAction(companyNumber)

          await(companyDetailsService.findCompany(companyNumber).value) shouldBe a[Left[_, _]]
        }

        "the call to get company details fails" in {
          testIsError(mockFindComapny(_)(Left(Error(""))))
        }

        "the call to get company details comes back with a 500 response" in {
          testIsError(mockFindComapny(_)(Right(HttpResponse(500, ""))))
        }

        "the call to get company details comes back with a 503 response" in {
          testIsError(mockFindComapny(_)(Right(HttpResponse(503, ""))))
        }

        "the call to get company details comes back with a 400 response" in {
          testIsError(mockFindComapny(_)(Right(HttpResponse(400, ""))))
        }

        "there is no JSON in the response body" in {
          testIsError(mockFindComapny(_)(Right(HttpResponse(200, ""))))
        }

        "the JSON cannot be parsed in the response body" in {
          testIsError(
            mockFindComapny(_)(
              Right(
                HttpResponse(
                  200,
                  Json.parse("""{ "a" : "b" }"""),
                  responseHeaders
                )
              )
            )
          )
        }

      }

      "return the details found" when {

        "the response from company details is OK and the json body can be parsed" in {

          val crn            = CRN("1234567")
          val companyDetails = CompanyHouseDetails(CompanyHouseName("Test Tech Ltd"))

          val json = Json.parse(s"""
               |{"company_name": { "name": "Test Tech Ltd" }}
               |""".stripMargin)

          mockFindComapny(CRN("1234567"))(Right(HttpResponse(200, json, responseHeaders)))
          val result = companyDetailsService.findCompany(crn)
          await(result.value) shouldBe Right(Some(companyDetails))
        }

        "the response from company details is Not Found and the json body is blank" in {
          val crn  = CRN("1234567")
          val json = Json.parse(s"""
                                   |{}
                                   |""".stripMargin)

          mockFindComapny(crn)(Right(HttpResponse(404, json, responseHeaders)))
          val result = companyDetailsService.findCompany(crn)
          await(result.value) shouldBe Right(None)
        }
      }

    }

  }

}
