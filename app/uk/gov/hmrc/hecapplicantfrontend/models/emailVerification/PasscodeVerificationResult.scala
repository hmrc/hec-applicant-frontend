/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.Eq
import play.api.libs.json.*

sealed trait PasscodeVerificationResult extends Product with Serializable

object PasscodeVerificationResult {

  case object Match extends PasscodeVerificationResult

  case object NoMatch extends PasscodeVerificationResult

  case object Expired extends PasscodeVerificationResult

  case object TooManyAttempts extends PasscodeVerificationResult

  implicit val eq: Eq[PasscodeVerificationResult] = Eq.fromUniversalEquals

  implicit val format: Format[PasscodeVerificationResult] = new Format[PasscodeVerificationResult] {
    override def reads(json: JsValue): JsResult[PasscodeVerificationResult] = json match {
      case JsString("Match")           => JsSuccess(Match)
      case JsString("NoMatch")         => JsSuccess(NoMatch)
      case JsString("Expired")         => JsSuccess(Expired)
      case JsString("TooManyAttempts") => JsSuccess(TooManyAttempts)
      case _                           => JsError(s"Unknown passcode verification result: ${json.toString()}")
    }

    override def writes(o: PasscodeVerificationResult): JsValue = JsString(o.toString)
  }

}
