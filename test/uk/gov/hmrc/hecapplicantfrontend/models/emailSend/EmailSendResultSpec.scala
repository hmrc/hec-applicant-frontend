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

package uk.gov.hmrc.hecapplicantfrontend.models.emailSend

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsString, Json}

class EmailSendResultSpec extends AnyWordSpec with Matchers {

  "EmailSendResult" should {

    "write to JSON" when {

      s"the email send result is ${EmailSendResult.EmailSent}" in {
        Json.toJson[EmailSendResult](EmailSendResult.EmailSent) shouldBe JsString("EmailSent")
      }

      s"the email send result is ${EmailSendResult.EmailSentFailure}" in {
        Json.toJson[EmailSendResult](EmailSendResult.EmailSentFailure) shouldBe JsString("EmailSentFailure")
      }
    }

    "read from JSON" when {

      s"the email send result is ${EmailSendResult.EmailSent}" in {
        JsString("EmailSent").as[EmailSendResult] shouldBe EmailSendResult.EmailSent
      }

      s"the email send result is ${EmailSendResult.EmailSentFailure}" in {
        JsString("EmailSentFailure").as[EmailSendResult] shouldBe EmailSendResult.EmailSentFailure
      }
    }
  }

}
