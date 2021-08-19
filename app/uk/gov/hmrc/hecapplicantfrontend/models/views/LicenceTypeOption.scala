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

package uk.gov.hmrc.hecapplicantfrontend.models.views

import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType

final case class LicenceTypeOption(messageKey: String, hintKey: Option[String])

object LicenceTypeOption {
  def licenceTypeOption(licenceType: LicenceType): LicenceTypeOption = licenceType match {
    case LicenceType.DriverOfTaxisAndPrivateHires  => LicenceTypeOption("driverOfTaxis", Some("driverOfTaxis.hint"))
    case LicenceType.ScrapMetalMobileCollector     => LicenceTypeOption("scrapMetalCollector", None)
    case LicenceType.ScrapMetalDealerSite          => LicenceTypeOption("scrapMetalDealer", None)
    case LicenceType.OperatorOfPrivateHireVehicles => LicenceTypeOption("operatorOfPrivateHireVehicles", None)
  }
}
