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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.functional.syntax.toInvariantFunctorOps
import play.api.libs.json.Format

import java.time.LocalDate
import java.time.format.DateTimeFormatter

final case class LicenceExpiryDate(value: LocalDate) extends AnyVal

object LicenceExpiryDate {

  private val dateFormatter = DateTimeFormatter.BASIC_ISO_DATE

  implicit val format: Format[LicenceExpiryDate] =
    implicitly[Format[String]]
      .inmap(s => LicenceExpiryDate(LocalDate.parse(s, dateFormatter)), d => dateFormatter.format(d.value))

}
