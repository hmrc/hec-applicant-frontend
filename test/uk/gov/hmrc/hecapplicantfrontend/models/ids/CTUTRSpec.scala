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

class CTUTRSpec extends AnyWordSpec with Matchers {

  "CTUTR" must {

    "have a format instance" in {
      val ctutr = CTUTR("utr")

      val json = Json.toJson(ctutr)
      json                       shouldBe JsString("utr")
      Json.fromJson[CTUTR](json) shouldBe JsSuccess(ctutr)
    }

    "have a method which validates an CT UTR from a string" in {
      CTUTR.fromString("")                 shouldBe None
      CTUTR.fromString("---")              shouldBe None
      CTUTR.fromString("12345")            shouldBe None
      CTUTR.fromString("1234567890")       shouldBe None
      CTUTR.fromString("a23456789b")       shouldBe None
      CTUTR.fromString("1234567895")       shouldBe Some(CTUTR("1234567895"))
      CTUTR.fromString(" 123456 789  5  ") shouldBe Some(CTUTR("1234567895"))
    }

  }

}
