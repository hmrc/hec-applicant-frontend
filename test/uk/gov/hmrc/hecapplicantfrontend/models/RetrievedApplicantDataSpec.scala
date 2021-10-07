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
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyJourneyData, CompanyLoginData, CompanyRetrievedData, IndividualJourneyData, IndividualLoginData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

class RetrievedApplicantDataSpec extends AnyWordSpec with Matchers {

  "RetrievedApplicantData" must {

    "perform JSON de/serialisation correctly" must {
      val dateOfBirthStr = "20001010"
      val dateOfBirth    = LocalDate.of(2000, 10, 10)

      val individualRetrievedData: RetrievedApplicantData =
        IndividualRetrievedData(
          IndividualLoginData(
            GGCredId("ggCredId"),
            NINO("nino"),
            Some(SAUTR("utr")),
            Name("first", "last"),
            DateOfBirth(dateOfBirth),
            Some(EmailAddress("email"))
          ),
          IndividualJourneyData.empty,
          List.empty
        )

      val individualJson = Json.parse(s"""{
          |"loginData" : {
          |  "ggCredId":"ggCredId",
          |  "nino":"nino",
          |  "sautr":"utr",
          |  "name":{
          |     "firstName":"first",
          |     "lastName":"last"
          |  },
          |  "dateOfBirth":"$dateOfBirthStr",
          |  "emailAddress":"email"
          |},  
          |"journeyData" : {},
          |"type":"Individual",
          |"unexpiredTaxChecks":[]
          |}""".stripMargin)

      val companyRetrievedData: RetrievedApplicantData =
        CompanyRetrievedData(
          CompanyLoginData(
            GGCredId("ggCredId"),
            Some(CTUTR("utr")),
            Some(EmailAddress("email"))
          ),
          CompanyJourneyData(
            Some(CompanyHouseName("Test Tech Ltd")),
            None,
            None
          ),
          List.empty
        )

      val companyJson = Json.parse("""{
          |"loginData": {
          |  "ggCredId":"ggCredId",
          |  "ctutr":"utr",
          |  "emailAddress":"email"
          |},
          |"journeyData" : { 
          |  "companyName" : "Test Tech Ltd"
          |},
          |"type":"Company",
          |"unexpiredTaxChecks":[]
          |}""".stripMargin)

      "serialize Individual retrieved data" in {
        Json.toJson(individualRetrievedData) shouldBe individualJson
      }

      "serialize Company retrieved data" in {
        Json.toJson(companyRetrievedData) shouldBe companyJson
      }

      "deserialize Individual retrieved data" in {
        Json.fromJson[RetrievedApplicantData](individualJson).get shouldBe individualRetrievedData
      }

      "deserialize Company retrieved data" when {
        "all fields are present" in {
          Json.fromJson[RetrievedApplicantData](companyJson).get shouldBe companyRetrievedData
        }

        "only mandatory fields are present" in {
          Json
            .fromJson[RetrievedApplicantData](
              Json.parse(
                """{"loginData" : { "ggCredId":"ggCredId" }, "journeyData" : { }, "type":"Company", "unexpiredTaxChecks":[]}"""
              )
            )
            .get shouldBe CompanyRetrievedData(
            CompanyLoginData(GGCredId("ggCredId"), None, None),
            CompanyJourneyData.empty,
            List.empty
          )
        }
      }
    }
  }

}
