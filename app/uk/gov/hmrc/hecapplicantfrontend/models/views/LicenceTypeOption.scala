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

package uk.gov.hmrc.hecapplicantfrontend.models.views

import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType

final case class LicenceTypeOption(messageKey: String, hintKey: Option[String])

object LicenceTypeOption {
  def licenceTypeOption(licenceType: LicenceType, isScotNIPrivateBeta: Option[Boolean]): LicenceTypeOption = {
    val isScotNI: Boolean = isScotNIPrivateBeta.getOrElse(false)

    licenceType match {
      case LicenceType.DriverOfTaxisAndPrivateHires =>
        val key  = "driverOfTaxis"
        val hint = s"$key.hint"
        LicenceTypeOption(key, Some(hint))

      case LicenceType.OperatorOfPrivateHireVehicles =>
        val key  = "operatorOfPrivateHireVehicles"
        val hint = if (isScotNI) Some(s"$key.hint") else None
        LicenceTypeOption(key, hint)

      case LicenceType.BookingOffice =>
        val key = "bookingOffice"
        LicenceTypeOption(key, Some(s"$key.hint"))

      case LicenceType.ScrapMetalMobileCollector =>
        val key = "scrapMetalCollector"
        if (isScotNI) LicenceTypeOption(key, Some(s"$key.hint"))
        else LicenceTypeOption(key, None)

      case LicenceType.ScrapMetalDealerSite =>
        val key  = "scrapMetalDealer"
        val hint = if (isScotNI) Some(s"$key.hint") else None
        LicenceTypeOption(key, hint)
    }
  }

}
