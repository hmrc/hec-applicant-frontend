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

package uk.gov.hmrc.hecapplicantfrontend.models.licence

import cats.Eq
import play.api.libs.json.{Format, Json, OFormat}

sealed trait LicenceType extends Product with Serializable

object LicenceType {

  case object DriverOfTaxisAndPrivateHires extends LicenceType

  case object OperatorOfPrivateHireVehicles extends LicenceType

  case object ScrapMetalMobileCollector extends LicenceType

  case object ScrapMetalDealerSite extends LicenceType

  case object BookingOffice extends LicenceType

  implicit val eq: Eq[LicenceType] = Eq.fromUniversalEquals

  implicit val format: Format[LicenceType] = Jsonx.formatSealed[LicenceType]

}
