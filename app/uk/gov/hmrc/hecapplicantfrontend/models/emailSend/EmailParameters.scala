/*
 * Copyright 2022 HM Revenue & Customs
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

import play.api.libs.json.{Json, OWrites}

//TODO discussion on data model for email parameters
final case class EmailParameters(
  name: String = "Dummy name",
  verificationLink: String //added to call dummy  template
  //currentDate: String, TODO uncomment for the real template
  //licenceTYpe: LicenceType, TODO uncomment for the real template
  //hecTaxCheckCode: String, TODO uncomment for the real template
  //expiresAfter: String TODO uncomment for the real template
)

object EmailParameters {
  implicit val writes: OWrites[EmailParameters] = Json.writes
}
