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
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

class LoginDataSpec extends AnyWordSpec with Matchers {

  "RetrievedApplicantData" must {

    "perform JSON de/serialisation correctly" must {
      val dateOfBirthStr = "20001010"
      val dateOfBirth    = LocalDate.of(2000, 10, 10)

      val individualLoginData: LoginData =
        IndividualLoginData(
          GGCredId("ggCredId"),
          NINO("nino"),
          Some(SAUTR("utr")),
          Name("first", "last"),
          DateOfBirth(dateOfBirth),
          Some(EmailAddress("email")),
          List.empty
        )

      val individualJson = Json.parse(s"""{
          |  "ggCredId":"ggCredId",
          |  "nino":"nino",
          |  "sautr":"utr",
          |  "name":{
          |     "firstName":"first",
          |     "lastName":"last"
          |  },
          |  "dateOfBirth":"$dateOfBirthStr",
          |  "emailAddress":"email",
          |  "type":"Individual",
          |  "unexpiredTaxChecks":[]
          |}""".stripMargin)

      val companyLoginData: LoginData =
        CompanyLoginData(
          GGCredId("ggCredId"),
          Some(CTUTR("utr")),
          Some(EmailAddress("email")),
          List.empty
        )

      val companyJson = Json.parse("""{
          |  "ggCredId":"ggCredId",
          |  "ctutr":"utr",
          |  "emailAddress":"email",
          |  "type":"Company",
          |  "unexpiredTaxChecks":[]
          |}""".stripMargin)

      "serialize Individual login data" in {
        Json.toJson(individualLoginData) shouldBe individualJson
      }

      "serialize Company login data" in {
        Json.toJson(companyLoginData) shouldBe companyJson
      }

      "deserialize Individual login data" in {
        Json.fromJson[IndividualLoginData](individualJson).get shouldBe individualLoginData
      }

      "deserialize Company login data" when {
        "all fields are present" in {
          Json.fromJson[CompanyLoginData](companyJson).get shouldBe companyLoginData
        }

        "only mandatory fields are present" in {
          Json
            .fromJson[CompanyLoginData](
              Json.parse(
                """{"ggCredId":"ggCredId", "type":"Company", "unexpiredTaxChecks":[]}"""
              )
            )
            .get shouldBe CompanyLoginData(GGCredId("ggCredId"), None, None, List.empty)
        }
      }
    }
  }

}
