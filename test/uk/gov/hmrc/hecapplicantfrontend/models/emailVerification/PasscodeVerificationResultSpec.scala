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

class PasscodeVerificationResultSpec extends AnyWordSpec with Matchers {

  "PasscodeVerificationResult" should {

    "write to JSON" when {

      s"the passcode verification result is ${PasscodeVerificationResult.Match}" in {
        Json.toJson[PasscodeVerificationResult](PasscodeVerificationResult.Match) shouldBe JsString("Match")
      }

      s"the passcode verification result is ${PasscodeVerificationResult.NoMatch}" in {
        Json.toJson[PasscodeVerificationResult](PasscodeVerificationResult.NoMatch) shouldBe JsString("NoMatch")
      }

      s"the passcode verification result is ${PasscodeVerificationResult.Expired}" in {
        Json.toJson[PasscodeVerificationResult](PasscodeVerificationResult.Expired) shouldBe JsString("Expired")
      }

      s"the passcode verification result is ${PasscodeVerificationResult.TooManyAttempts}" in {
        Json.toJson[PasscodeVerificationResult](PasscodeVerificationResult.TooManyAttempts) shouldBe JsString(
          "TooManyAttempts"
        )
      }
    }

    "read from JSON" when {

      s"the passcode verification result is ${PasscodeVerificationResult.Match}" in {
        JsString("Match").as[PasscodeVerificationResult] shouldBe PasscodeVerificationResult.Match
      }

      s"the passcode verification result is ${PasscodeVerificationResult.NoMatch}" in {
        JsString("NoMatch").as[PasscodeVerificationResult] shouldBe PasscodeVerificationResult.NoMatch
      }

      s"the passcode verification result is ${PasscodeVerificationResult.Expired}" in {
        JsString("Expired").as[PasscodeVerificationResult] shouldBe PasscodeVerificationResult.Expired
      }

      s"the passcode verification result is ${PasscodeVerificationResult.TooManyAttempts}" in {
        JsString("TooManyAttempts").as[PasscodeVerificationResult] shouldBe PasscodeVerificationResult.TooManyAttempts
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the passcode verification result is not recognised" in {
        js.validate[PasscodeVerificationResult] shouldBe JsError(
          s"Unknown passcode verification result: ${js.toString()}"
        )

      }
    }
  }

}
