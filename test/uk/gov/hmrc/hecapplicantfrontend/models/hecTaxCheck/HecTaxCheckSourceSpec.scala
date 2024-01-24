/*
 * Copyright 2024 HM Revenue & Customs
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

package uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsString, Json}

class HecTaxCheckSourceSpec extends AnyWordSpec with Matchers {

  "HecTaxCheckSource" should {

    "write to JSON" when {

      s"the hec tax check source is ${HECTaxCheckSource.Digital}" in {
        Json.toJson[HECTaxCheckSource](HECTaxCheckSource.Digital) shouldBe JsString("Digital")
      }
    }

    "read from JSON" when {

      s"the hec tax check source is ${HECTaxCheckSource.Digital}" in {
        JsString("Digital").as[HECTaxCheckSource] shouldBe HECTaxCheckSource.Digital
      }
    }
  }

}
