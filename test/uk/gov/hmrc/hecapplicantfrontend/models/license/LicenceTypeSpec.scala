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
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType

class LicenceTypeSpec extends AnyWordSpec with Matchers {

  "LicenceType" should {

    "write to JSON" when {

      s"the licence type is ${LicenceType.DriverOfTaxisAndPrivateHires}" in {
        Json.toJson[LicenceType](LicenceType.DriverOfTaxisAndPrivateHires) shouldBe JsString(
          "DriverOfTaxisAndPrivateHires"
        )
      }

      s"the licence type is ${LicenceType.OperatorOfPrivateHireVehicles}" in {
        Json.toJson[LicenceType](LicenceType.OperatorOfPrivateHireVehicles) shouldBe JsString(
          "OperatorOfPrivateHireVehicles"
        )
      }

      s"the licence type is ${LicenceType.ScrapMetalMobileCollector}" in {
        Json.toJson[LicenceType](LicenceType.ScrapMetalMobileCollector) shouldBe JsString("ScrapMetalMobileCollector")
      }

      s"the licence type is ${LicenceType.ScrapMetalDealerSite}" in {
        Json.toJson[LicenceType](LicenceType.ScrapMetalDealerSite) shouldBe JsString("ScrapMetalDealerSite")
      }

      s"the licence type is ${LicenceType.BookingOffice}" in {
        Json.toJson[LicenceType](LicenceType.BookingOffice) shouldBe JsString("BookingOffice")
      }
    }

    "read from JSON" when {

      s"the licence type is ${LicenceType.DriverOfTaxisAndPrivateHires}" in {
        JsString("DriverOfTaxisAndPrivateHires").as[LicenceType] shouldBe LicenceType.DriverOfTaxisAndPrivateHires
      }

      s"the licence type is ${LicenceType.OperatorOfPrivateHireVehicles}" in {
        JsString("OperatorOfPrivateHireVehicles").as[LicenceType] shouldBe LicenceType.OperatorOfPrivateHireVehicles
      }

      s"the licence type is ${LicenceType.ScrapMetalMobileCollector}" in {
        JsString("ScrapMetalMobileCollector").as[LicenceType] shouldBe LicenceType.ScrapMetalMobileCollector
      }

      s"the licence type is ${LicenceType.ScrapMetalDealerSite}" in {
        JsString("ScrapMetalDealerSite").as[LicenceType] shouldBe LicenceType.ScrapMetalDealerSite
      }

      s"the licence type is ${LicenceType.BookingOffice}" in {
        JsString("BookingOffice").as[LicenceType] shouldBe LicenceType.BookingOffice
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the licence type is not recognised" in {
        js.validate[LicenceType] shouldBe JsError(s"Unknown licence type: ${js.toString()}")

      }
    }
  }

}
