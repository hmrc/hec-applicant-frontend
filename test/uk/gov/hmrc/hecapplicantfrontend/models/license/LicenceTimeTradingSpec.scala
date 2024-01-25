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
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceTimeTrading

class LicenceTimeTradingSpec extends AnyWordSpec with Matchers {

  "LicenceTimeTrading" should {

    "write to JSON" when {

      s"the licence time trading is ${LicenceTimeTrading.ZeroToTwoYears}" in {
        Json.toJson[LicenceTimeTrading](LicenceTimeTrading.ZeroToTwoYears) shouldBe JsString("ZeroToTwoYears")
      }

      s"the licence time trading is ${LicenceTimeTrading.TwoToFourYears}" in {
        Json.toJson[LicenceTimeTrading](LicenceTimeTrading.TwoToFourYears) shouldBe JsString("TwoToFourYears")
      }

      s"the licence time trading is ${LicenceTimeTrading.FourToEightYears}" in {
        Json.toJson[LicenceTimeTrading](LicenceTimeTrading.FourToEightYears) shouldBe JsString("FourToEightYears")
      }

      s"the licence time trading is ${LicenceTimeTrading.EightYearsOrMore}" in {
        Json.toJson[LicenceTimeTrading](LicenceTimeTrading.EightYearsOrMore) shouldBe JsString("EightYearsOrMore")
      }
    }

    "read from JSON" when {

      s"the licence time trading is ${LicenceTimeTrading.ZeroToTwoYears}" in {
        JsString("ZeroToTwoYears").as[LicenceTimeTrading] shouldBe LicenceTimeTrading.ZeroToTwoYears
      }

      s"the licence time trading is ${LicenceTimeTrading.TwoToFourYears}" in {
        JsString("TwoToFourYears").as[LicenceTimeTrading] shouldBe LicenceTimeTrading.TwoToFourYears
      }

      s"the licence time trading is ${LicenceTimeTrading.FourToEightYears}" in {
        JsString("FourToEightYears").as[LicenceTimeTrading] shouldBe LicenceTimeTrading.FourToEightYears
      }

      s"the licence time trading is ${LicenceTimeTrading.EightYearsOrMore}" in {
        JsString("EightYearsOrMore").as[LicenceTimeTrading] shouldBe LicenceTimeTrading.EightYearsOrMore
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the email type is not recognised" in {
        js.validate[LicenceTimeTrading] shouldBe JsError(s"Unknown licence time trading period: ${js.toString()}")

      }
    }
  }
}
