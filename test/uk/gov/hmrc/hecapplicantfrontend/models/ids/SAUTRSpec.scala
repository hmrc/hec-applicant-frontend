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

package uk.gov.hmrc.hecapplicantfrontend.models.ids

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsString, JsSuccess, Json}

class SAUTRSpec extends AnyWordSpec with Matchers {

  "SAUTR" must {

    "have a format instance" in {
      val sautr = SAUTR("utr")

      val json = Json.toJson(sautr)
      json                       shouldBe JsString("utr")
      Json.fromJson[SAUTR](json) shouldBe JsSuccess(sautr)
    }

    "have a method which validates an SA UTR from a string" in {
      SAUTR.fromString("")                 shouldBe None
      SAUTR.fromString("---")              shouldBe None
      SAUTR.fromString("12345")            shouldBe None
      SAUTR.fromString("1234567890")       shouldBe None
      SAUTR.fromString("a23456789b")       shouldBe None
      SAUTR.fromString("1234567895")       shouldBe Some(SAUTR("1234567895"))
      SAUTR.fromString(" 123456 789  5  ") shouldBe Some(SAUTR("1234567895"))
    }

  }

}
