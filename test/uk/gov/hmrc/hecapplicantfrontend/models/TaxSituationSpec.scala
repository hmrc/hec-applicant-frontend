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

package uk.gov.hmrc.hecapplicantfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsError, JsString, Json}

class TaxSituationSpec extends AnyWordSpec with Matchers {

  "TaxSituation" should {

    "write to JSON" when {

      s"the tax situation type is ${TaxSituation.PAYE}" in {
        Json.toJson[TaxSituation](TaxSituation.PAYE) shouldBe JsString("PAYE")
      }

      s"the tax situation type is ${TaxSituation.SA}" in {
        Json.toJson[TaxSituation](TaxSituation.SA) shouldBe JsString("SA")
      }

      s"the tax situation type is ${TaxSituation.SAPAYE}" in {
        Json.toJson[TaxSituation](TaxSituation.SAPAYE) shouldBe JsString("SAPAYE")
      }

      s"the tax situation type is ${TaxSituation.NotChargeable}" in {
        Json.toJson[TaxSituation](TaxSituation.NotChargeable) shouldBe JsString("NotChargeable")
      }
    }

    "read from JSON" when {

      s"the tax situation type is ${TaxSituation.PAYE}" in {
        JsString("PAYE").as[TaxSituation] shouldBe TaxSituation.PAYE
      }

      s"the tax situation type is ${TaxSituation.SA}" in {
        JsString("SA").as[TaxSituation] shouldBe TaxSituation.SA
      }

      s"the tax situation type is ${TaxSituation.SAPAYE}" in {
        JsString("SAPAYE").as[TaxSituation] shouldBe TaxSituation.SAPAYE
      }

      s"the tax situation type is ${TaxSituation.NotChargeable}" in {
        JsString("NotChargeable").as[TaxSituation] shouldBe TaxSituation.NotChargeable
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the email type is not recognised" in {
        js.validate[TaxSituation] shouldBe JsError(s"Unknown tax situation: ${js.toString()}")

      }
    }
  }

}
