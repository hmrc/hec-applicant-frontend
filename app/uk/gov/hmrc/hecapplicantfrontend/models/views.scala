package uk.gov.hmrc.hecapplicantfrontend

import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType

package object views {

  case class LicenceTypeOption(messageKey: String, hintKey: Option[String])

  def licenceTypeOption(licenceType: LicenceType): LicenceTypeOption = licenceType match {
    case LicenceType.DriverOfTaxisAndPrivateHires => LicenceTypeOption("driverOfTaxis", Some("driverOfTaxis.hint"))
    case LicenceType.ScrapMetalMobileCollector => LicenceTypeOption("scrapMetalCollector", None)
    case LicenceType.ScrapMetalDealerSite => LicenceTypeOption("scrapMetalDealer", None)
    case LicenceType.OperatorOfPrivateHireVehicles => LicenceTypeOption("operatorOfPrivateHireVehicles", None)
  }
}
