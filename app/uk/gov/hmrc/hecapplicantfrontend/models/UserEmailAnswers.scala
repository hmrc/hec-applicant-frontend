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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.functional.syntax.*
import play.api.libs.json.{OFormat, __}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}

final case class UserEmailAnswers(
  userSelectedEmail: UserSelectedEmail,
  passcodeRequestResult: Option[PasscodeRequestResult],
  passcode: Option[Passcode],
  passcodeVerificationResult: Option[PasscodeVerificationResult],
  emailSendResult: Option[EmailSendResult]
)

object UserEmailAnswers {
  implicit val format: OFormat[UserEmailAnswers] = (
    (__ \ "userSelectedEmail").format[UserSelectedEmail] and
      (__ \ "passcodeRequestResult").formatNullable[PasscodeRequestResult] and
      (__ \ "passcode").formatNullable[Passcode] and
      (__ \ "passcodeVerificationResult").formatNullable[PasscodeVerificationResult] and
      (__ \ "emailSendResult").formatNullable[EmailSendResult]
  )(UserEmailAnswers.apply, o => Tuple.fromProductTyped(o))
}
