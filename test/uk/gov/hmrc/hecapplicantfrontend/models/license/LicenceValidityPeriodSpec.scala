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

package uk.gov.hmrc.hecapplicantfrontend.models.license

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsError, JsString, Json}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod

class LicenceValidityPeriodSpec extends AnyWordSpec with Matchers {

  "LicenceValidityPeriod" should {

    "write to JSON" when {

      s"the licence validity period is ${LicenceValidityPeriod.UpToOneYear}" in {
        Json.toJson[LicenceValidityPeriod](LicenceValidityPeriod.UpToOneYear) shouldBe JsString("UpToOneYear")
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToTwoYears}" in {
        Json.toJson[LicenceValidityPeriod](LicenceValidityPeriod.UpToTwoYears) shouldBe JsString("UpToTwoYears")
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToThreeYears}" in {
        Json.toJson[LicenceValidityPeriod](LicenceValidityPeriod.UpToThreeYears) shouldBe JsString("UpToThreeYears")
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToFourYears}" in {
        Json.toJson[LicenceValidityPeriod](LicenceValidityPeriod.UpToFourYears) shouldBe JsString("UpToFourYears")
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToFiveYears}" in {
        Json.toJson[LicenceValidityPeriod](LicenceValidityPeriod.UpToFiveYears) shouldBe JsString("UpToFiveYears")
      }
    }

    "read from JSON" when {

      s"the licence validity period is ${LicenceValidityPeriod.UpToOneYear}" in {
        JsString("UpToOneYear").as[LicenceValidityPeriod] shouldBe LicenceValidityPeriod.UpToOneYear
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToTwoYears}" in {
        JsString("UpToTwoYears").as[LicenceValidityPeriod] shouldBe LicenceValidityPeriod.UpToTwoYears
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToThreeYears}" in {
        JsString("UpToThreeYears").as[LicenceValidityPeriod] shouldBe LicenceValidityPeriod.UpToThreeYears
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToFourYears}" in {
        JsString("UpToFourYears").as[LicenceValidityPeriod] shouldBe LicenceValidityPeriod.UpToFourYears
      }

      s"the licence validity period is ${LicenceValidityPeriod.UpToFiveYears}" in {
        JsString("UpToFiveYears").as[LicenceValidityPeriod] shouldBe LicenceValidityPeriod.UpToFiveYears
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the licence validity period is not recognised" in {
        js.validate[LicenceValidityPeriod] shouldBe JsError(s"Unknown licence validity period: ${js.toString()}")

      }
    }
  }

}
