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

class EmailTypeSpec extends AnyWordSpec with Matchers {

  "EmailType" should {

    "write to JSON" when {

      s"the email type is ${EmailType.GGEmail}" in {
        Json.toJson[EmailType](EmailType.GGEmail) shouldBe JsString("GGEmail")
      }

      s"the email type is ${EmailType.DifferentEmail}" in {
        Json.toJson[EmailType](EmailType.DifferentEmail) shouldBe JsString("DifferentEmail")
      }
    }

    "read from JSON" when {

      s"the email type is ${EmailType.GGEmail}" in {
        JsString("GGEmail").as[EmailType] shouldBe EmailType.GGEmail
      }

      s"the email type is ${EmailType.DifferentEmail}" in {
        JsString("DifferentEmail").as[EmailType] shouldBe EmailType.DifferentEmail
      }
    }
    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the email type is not recognised" in {
        js.validate[EmailType] shouldBe JsError(s"Unknown email type: ${js.toString()}")

      }
    }
  }

}
