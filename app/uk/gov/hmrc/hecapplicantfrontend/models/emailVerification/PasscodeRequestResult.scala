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

sealed trait PasscodeRequestResult extends Product with Serializable

object PasscodeRequestResult {

  case object PasscodeSent extends PasscodeRequestResult

  case object EmailAddressAlreadyVerified extends PasscodeRequestResult

  case object MaximumNumberOfEmailsExceeded extends PasscodeRequestResult

  case object BadEmailAddress extends PasscodeRequestResult

  implicit val eq: Eq[PasscodeRequestResult] = Eq.fromUniversalEquals

  implicit val format: Format[PasscodeRequestResult] = new Format[PasscodeRequestResult] {
    override def reads(json: JsValue): JsResult[PasscodeRequestResult] = json match {
      case JsString("PasscodeSent")                  => JsSuccess(PasscodeSent)
      case JsString("EmailAddressAlreadyVerified")   => JsSuccess(EmailAddressAlreadyVerified)
      case JsString("MaximumNumberOfEmailsExceeded") => JsSuccess(MaximumNumberOfEmailsExceeded)
      case JsString("BadEmailAddress")               => JsSuccess(BadEmailAddress)
      case _                                         => JsError(s"Unknown passcode request result: ${json.toString()}")
    }

    override def writes(o: PasscodeRequestResult): JsValue = JsString(o.toString)
  }

}
