/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.hecapplicantfrontend.testonly.models

import play.api.libs.json.{Json, Writes}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, HECTaxCheckCode, HECTaxCheckSource}
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.EitherUtils.eitherFormat

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, ZonedDateTime}

final case class SaveTaxCheckRequest(
  taxCheckCode: HECTaxCheckCode,
  ggCredId: GGCredId,
  licenceType: LicenceType,
  verifier: Either[CRN, DateOfBirth],
  expiresAfter: LocalDate,
  createDate: ZonedDateTime,
  taxCheckStartDateTime: ZonedDateTime,
  isExtracted: Boolean,
  source: HECTaxCheckSource
)

object SaveTaxCheckRequest {

  implicit val localDateWrites: Writes[LocalDate] =
    Writes.temporalWrites[LocalDate, DateTimeFormatter](DateTimeFormatter.BASIC_ISO_DATE)

  implicit val writes: Writes[SaveTaxCheckRequest] = Json.writes

}
