package uk.gov.hmrc.hecapplicantfrontend.models

import cats.Eq
import julienrf.json.derived
import play.api.libs.json.OFormat

sealed trait LicenceValidityPeriod extends Product with Serializable
object LicenceValidityPeriod {
  case object UpToOneYear extends LicenceValidityPeriod
  case object UpToTwoYears extends LicenceValidityPeriod
  case object UpToThreeYears extends LicenceValidityPeriod
  case object UpToFourYears extends LicenceValidityPeriod
  case object UpToFiveYears extends LicenceValidityPeriod

  implicit val eq: Eq[LicenceValidityPeriod] = Eq.fromUniversalEquals

  implicit val format: OFormat[LicenceTimeTrading] = derived.oformat()
}