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

import cats.Eq
import play.api.libs.json._

sealed trait EmailType extends Product with Serializable

object EmailType {

  case object GGEmail extends EmailType
  case object DifferentEmail extends EmailType

  implicit val eq: Eq[EmailType] = Eq.fromUniversalEquals

  implicit val format: Format[EmailType] = new Format[EmailType] {
    override def reads(json: JsValue): JsResult[EmailType] = json match {
      case JsString("GGEmail")        => JsSuccess(GGEmail)
      case JsString("DifferentEmail") => JsSuccess(DifferentEmail)
      case _                          => JsError(s"Unknown email type: ${json.toString()}")
    }

    override def writes(o: EmailType): JsValue = JsString(o.toString)
  }

}

final case class UserSelectedEmail(emailType: EmailType, emailAddress: EmailAddress)

object UserSelectedEmail {
  implicit val format: OFormat[UserSelectedEmail] = Json.format[UserSelectedEmail]
}
