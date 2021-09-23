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

package uk.gov.hmrc.hecapplicantfrontend.models

import java.time.LocalDate

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.ApplicantDetails.{CompanyApplicantDetails, IndividualApplicantDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.HECTaxCheckData.{CompanyHECTaxCheckData, IndividualHECTaxCheckData}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDetails.{CompanyTaxDetails, IndividualTaxDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceDetails, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}

class HECTaxCheckDataSpec extends AnyWordSpec with Matchers {

  "HECTaxCheckData" must {

    "perform JSON de/serialisation correctly" must {
      val dateOfBirthStr = "20001010"
      val dateOfBirth    = LocalDate.of(2000, 10, 10)

      val individualTaxCheckData: HECTaxCheckData =
        IndividualHECTaxCheckData(
          IndividualApplicantDetails(
            GGCredId("ggCredId"),
            Name("first", "last"),
            DateOfBirth(dateOfBirth)
          ),
          LicenceDetails(
            LicenceType.ScrapMetalMobileCollector,
            LicenceTimeTrading.EightYearsOrMore,
            LicenceValidityPeriod.UpToThreeYears
          ),
          IndividualTaxDetails(
            NINO("nino"),
            Some(SAUTR("utr")),
            TaxSituation.SA,
            Some(IncomeDeclared.Yes)
          )
        )

      val individualJson = Json.parse(s"""{
          | "applicantDetails":{
          |    "ggCredId":"ggCredId",
          |    "name":{
          |      "firstName":"first",
          |      "lastName":"last"
          |    },
          |    "dateOfBirth":"$dateOfBirthStr"
          | },
          | "licenceDetails":{
          |    "licenceType":"ScrapMetalMobileCollector",
          |    "licenceTimeTrading":"EightYearsOrMore",
          |    "licenceValidityPeriod":"UpToThreeYears"
          | },
          | "taxDetails":{
          |    "nino":"nino",
          |    "sautr":"utr",
          |    "taxSituation":"SA",
          |    "saIncomeDeclared":"Yes"
          | },
          | "type":"Individual"
          |}""".stripMargin)

      val companyTaxCheckData: HECTaxCheckData =
        CompanyHECTaxCheckData(
          CompanyApplicantDetails(
            GGCredId("ggCredId")
          ),
          LicenceDetails(
            LicenceType.ScrapMetalMobileCollector,
            LicenceTimeTrading.EightYearsOrMore,
            LicenceValidityPeriod.UpToThreeYears
          ),
          CompanyTaxDetails(
            CTUTR("utr")
          )
        )

      val companyJson = Json.parse("""{
          | "applicantDetails":{
          |   "ggCredId":"ggCredId"
          | },
          | "licenceDetails":{
          |   "licenceType":"ScrapMetalMobileCollector",
          |   "licenceTimeTrading":"EightYearsOrMore",
          |   "licenceValidityPeriod":"UpToThreeYears"
          | },
          | "taxDetails":{
          |   "ctutr":"utr"
          | },
          | "type":"Company"
          |}""".stripMargin)

      "serialize Individual data" in {
        Json.toJson(individualTaxCheckData) shouldBe individualJson
      }

      "serialize Company data" in {
        Json.toJson(companyTaxCheckData) shouldBe companyJson
      }

      "deserialize Individual data" in {
        Json.fromJson[HECTaxCheckData](individualJson).get shouldBe individualTaxCheckData
      }

      "deserialize Company data" in {
        Json.fromJson[HECTaxCheckData](companyJson).get shouldBe companyTaxCheckData
      }
    }
  }

}
