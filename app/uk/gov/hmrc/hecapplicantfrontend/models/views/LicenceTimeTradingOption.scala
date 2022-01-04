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

import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceTimeTrading

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
