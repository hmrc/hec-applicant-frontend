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

package uk.gov.hmrc.hecapplicantfrontend.models.emailVerification

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsError, JsString, Json}

class PasscodeRequestResultSpec extends AnyWordSpec with Matchers {

  "PasscodeRequestResult" should {

    "write to JSON" when {

      s"the passcode request result is ${PasscodeRequestResult.PasscodeSent}" in {
        Json.toJson[PasscodeRequestResult](PasscodeRequestResult.PasscodeSent) shouldBe JsString("PasscodeSent")
      }

      s"the passcode request result is ${PasscodeRequestResult.EmailAddressAlreadyVerified}" in {
        Json.toJson[PasscodeRequestResult](PasscodeRequestResult.EmailAddressAlreadyVerified) shouldBe JsString(
          "EmailAddressAlreadyVerified"
        )
      }

      s"the passcode request result is ${PasscodeRequestResult.MaximumNumberOfEmailsExceeded}" in {
        Json.toJson[PasscodeRequestResult](PasscodeRequestResult.MaximumNumberOfEmailsExceeded) shouldBe JsString(
          "MaximumNumberOfEmailsExceeded"
        )
      }

      s"the passcode request result is ${PasscodeRequestResult.BadEmailAddress}" in {
        Json.toJson[PasscodeRequestResult](PasscodeRequestResult.BadEmailAddress) shouldBe JsString("BadEmailAddress")
      }
    }

    "read from JSON" when {

      s"the passcode request result is ${PasscodeRequestResult.PasscodeSent}" in {
        JsString("PasscodeSent").as[PasscodeRequestResult] shouldBe PasscodeRequestResult.PasscodeSent
      }

      s"the passcode request result is ${PasscodeRequestResult.EmailAddressAlreadyVerified}" in {
        JsString("EmailAddressAlreadyVerified")
          .as[PasscodeRequestResult] shouldBe PasscodeRequestResult.EmailAddressAlreadyVerified
      }

      s"the passcode request result is ${PasscodeRequestResult.MaximumNumberOfEmailsExceeded}" in {
        JsString("MaximumNumberOfEmailsExceeded")
          .as[PasscodeRequestResult] shouldBe PasscodeRequestResult.MaximumNumberOfEmailsExceeded
      }

      s"the passcode request result is ${PasscodeRequestResult.BadEmailAddress}" in {
        JsString("BadEmailAddress").as[PasscodeRequestResult] shouldBe PasscodeRequestResult.BadEmailAddress
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the email type is not recognised" in {
        js.validate[PasscodeRequestResult] shouldBe JsError(s"Unknown passcode request result: ${js.toString()}")

      }
    }
  }

}
