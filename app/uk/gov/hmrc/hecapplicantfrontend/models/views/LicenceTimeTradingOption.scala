package uk.gov.hmrc.hecapplicantfrontend.models.views

import uk.gov.hmrc.hecapplicantfrontend.models.{LicenceTimeTrading, LicenceType}

final case class LicenceTimeTradingOption (messageKey: String, hintKey: Option[String])
object LicenceTimeTradingOption {
  def licenceTimeTradingOption(licenceTimeTrading: LicenceTimeTrading): LicenceTypeOption = licenceTimeTrading match {
    case LicenceTimeTrading.ZeroToTwoYears  => LicenceTypeOption("zeroToTwoYears", None)
    case LicenceTimeTrading.TwoToFourYears     => LicenceTypeOption("twoToFourYears", None)
    case LicenceTimeTrading.FourToEightYears          => LicenceTypeOption("fourToEightYears", None)
    case LicenceTimeTrading.EightYearsOrMore => LicenceTypeOption("eightYearsOrMore", None)
  }
}