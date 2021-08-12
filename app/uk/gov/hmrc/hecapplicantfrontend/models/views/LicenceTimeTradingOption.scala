package uk.gov.hmrc.hecapplicantfrontend.models.views

import uk.gov.hmrc.hecapplicantfrontend.models.{LicenceTimeTrading, LicenceType}

final case class LicenceTimeTradingOption(messageKey: String)
object LicenceTimeTradingOption {
  def licenceTimeTradingOption(licenceTimeTrading: LicenceTimeTrading): LicenceTimeTradingOption =
    licenceTimeTrading match {
      case LicenceTimeTrading.ZeroToTwoYears   => LicenceTimeTradingOption("zeroToTwoYears")
      case LicenceTimeTrading.TwoToFourYears   => LicenceTimeTradingOption("twoToFourYears")
      case LicenceTimeTrading.FourToEightYears => LicenceTimeTradingOption("fourToEightYears")
      case LicenceTimeTrading.EightYearsOrMore => LicenceTimeTradingOption("eightYearsOrMore")
    }
}
