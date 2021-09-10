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

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait IncomeConfirmation extends Product with Serializable

object IncomeConfirmation {

  case object Yes extends IncomeConfirmation

  case object No extends IncomeConfirmation

  implicit val eq: Eq[IncomeConfirmation] = Eq.fromUniversalEquals

  implicit val format: OFormat[IncomeConfirmation] = derived.oformat()

  val values: List[IncomeConfirmation] = List(Yes, No)

}
