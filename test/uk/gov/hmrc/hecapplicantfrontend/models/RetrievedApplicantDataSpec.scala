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
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

class RetrievedApplicantDataSpec extends AnyWordSpec with Matchers {

  "RetrievedApplicantData" must {

    "perform JSON de/serialisation correctly" must {
      val dateOfBirthStr = "20001010"
      val dateOfBirth    = LocalDate.of(2000, 10, 10)

      val individualRetrievedData: RetrievedApplicantData =
        IndividualRetrievedData(
          GGCredId("ggCredId"),
          NINO("nino"),
          Some(SAUTR("utr")),
          Name("first", "last"),
          DateOfBirth(dateOfBirth),
          Some(EmailAddress("email")),
          None,
          List.empty
        )

      val individualJson = Json.parse(s"""{
          |"ggCredId":"ggCredId",
          |"nino":"nino",
          |"sautr":"utr",
          |"name":{
          |   "firstName":"first",
          |   "lastName":"last"
          |},
          |"dateOfBirth":"$dateOfBirthStr",
          |"emailAddress":"email",
          |"type":"Individual",
          |"unexpiredTaxChecks":[]
          |}""".stripMargin)

      val companyRetrievedData: RetrievedApplicantData =
        CompanyRetrievedData(
          GGCredId("ggCredId"),
          Some(CTUTR("utr")),
          Some(EmailAddress("email")),
          Some(CompanyHouseName("Test Tech Ltd")),
          None,
          None,
          List.empty
        )

      val companyJson = Json.parse("""{
          |"ggCredId":"ggCredId",
          |"ctutr":"utr",
          |"emailAddress":"email",
          |"type":"Company",
          |"companyName":"Test Tech Ltd",
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
              Json.parse("""{"ggCredId":"ggCredId", "type":"Company", "unexpiredTaxChecks":[]}""")
            )
            .get shouldBe CompanyRetrievedData(GGCredId("ggCredId"), None, None, None, None, None, List.empty)
        }
      }
    }
  }

}
