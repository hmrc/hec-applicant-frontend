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

import play.api.libs.functional.syntax.toInvariantFunctorOps
import play.api.libs.json.Format

import java.time.LocalDate
import java.time.format.DateTimeFormatter

final case class DateOfBirth(value: LocalDate) extends AnyVal

object DateOfBirth {

  private val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE

  implicit val format: Format[DateOfBirth] =
    implicitly[Format[String]]
      .inmap(s => DateOfBirth(LocalDate.parse(s, dateFormatter)), d => dateFormatter.format(d.value))

}
