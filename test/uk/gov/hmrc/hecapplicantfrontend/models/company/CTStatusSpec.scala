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

package uk.gov.hmrc.hecapplicantfrontend.models.company

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsError, JsString, Json}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTStatus

class CTStatusSpec extends AnyWordSpec with Matchers {

  "CTStatus" should {

    "write to JSON" when {

      s"the ct status is ${CTStatus.ReturnFound}" in {
        Json.toJson[CTStatus](CTStatus.ReturnFound) shouldBe JsString("ReturnFound")
      }

      s"the ct status is ${CTStatus.NoticeToFileIssued}" in {
        Json.toJson[CTStatus](CTStatus.NoticeToFileIssued) shouldBe JsString("NoticeToFileIssued")
      }

      s"the ct status is ${CTStatus.NoReturnFound}" in {
        Json.toJson[CTStatus](CTStatus.NoReturnFound) shouldBe JsString("NoReturnFound")
      }
    }

    "read from JSON" when {

      s"the ct status is ${CTStatus.ReturnFound}" in {
        JsString("ReturnFound").as[CTStatus] shouldBe CTStatus.ReturnFound
      }

      s"the ct status is ${CTStatus.NoticeToFileIssued}" in {
        JsString("NoticeToFileIssued").as[CTStatus] shouldBe CTStatus.NoticeToFileIssued
      }

      s"the ct status is ${CTStatus.NoReturnFound}" in {
        JsString("NoReturnFound").as[CTStatus] shouldBe CTStatus.NoReturnFound
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the email type is not recognised" in {
        js.validate[CTStatus] shouldBe JsError(s"Unknown CT status: ${js.toString()}")

      }
    }
  }

}
