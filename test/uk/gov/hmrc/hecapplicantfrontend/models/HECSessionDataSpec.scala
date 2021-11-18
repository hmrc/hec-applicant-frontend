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
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

class HECSessionDataSpec extends AnyWordSpec with Matchers {

  "HECSessionData" must {

    "perform JSON de/serialisation correctly" must {
      val dateOfBirthStr = "20001010"
      val dateOfBirth    = LocalDate.of(2000, 10, 10)

      val individualLoginData =
        IndividualLoginData(
          GGCredId("ggCredId"),
          NINO("nino"),
          Some(SAUTR("utr")),
          Name("first", "last"),
          DateOfBirth(dateOfBirth),
          Some(EmailAddress("email"))
        )

      val individualSession: HECSession = IndividualHECSession.newSession(individualLoginData)

      val individualJson = Json.parse(s"""{
          |  "loginData" : {
          |    "ggCredId":"ggCredId",
          |    "nino":"nino",
          |    "sautr":"utr",
          |    "name":{
          |       "firstName":"first",
          |       "lastName":"last"
          |    },
          |    "dateOfBirth":"$dateOfBirthStr",
          |    "emailAddress":"email"
          |  },
          |  "retrievedJourneyData" : { },
          |  "userAnswers" : { "type" : "Incomplete"  },
          |  "unexpiredTaxChecks" : [],
          |  "type":"Individual"
          |}""".stripMargin)

      val companyLoginData =
        CompanyLoginData(
          GGCredId("ggCredId"),
          Some(CTUTR("utr")),
          Some(EmailAddress("email"))
        )

      val companySession: HECSession = CompanyHECSession.newSession(companyLoginData)

      val companyJson = Json.parse("""{
          |  "loginData" : {
          |    "ggCredId":"ggCredId",
          |    "ctutr":"utr",
          |    "emailAddress":"email"
          |    },
          |  "retrievedJourneyData" : { },
          |  "userAnswers" : { "type" : "Incomplete" },
          |  "unexpiredTaxChecks" : [],
          |  "crnBlocked" : false,
          |  "type":"Company"
          |}""".stripMargin)

      "serialize Individual session data" in {
        Json.toJson(individualSession) shouldBe individualJson
      }

      "serialize Company session data" in {
        Json.toJson(companySession) shouldBe companyJson
      }

      "deserialize Individual session data" in {
        Json.fromJson[HECSession](individualJson).get shouldBe individualSession
      }

      "deserialize Company session data" in {
        Json.fromJson[HECSession](companyJson).get shouldBe companySession
      }
    }
  }

}
