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

package uk.gov.hmrc.hecapplicantfrontend.utils

import uk.gov.hmrc.hecapplicantfrontend.models.{EntityType, TaxSituation, YesNoAnswer}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}

object Fixtures {

  def incompleteUserAnswers(
    licenceType: Option[LicenceType] = None,
    licenceTimeTrading: Option[LicenceTimeTrading] = None,
    licenceValidityPeriod: Option[LicenceValidityPeriod] = None,
    taxSituation: Option[TaxSituation] = None,
    saIncomeDeclared: Option[YesNoAnswer] = None,
    entityType: Option[EntityType] = None,
    crn: Option[CRN] = None,
    companyDetailsConfirmed: Option[YesNoAnswer] = None,
    chargeableForCT: Option[YesNoAnswer] = None,
    ctIncomeDeclared: Option[YesNoAnswer] = None
  ): IncompleteUserAnswers = IncompleteUserAnswers(
    licenceType = licenceType,
    licenceTimeTrading = licenceTimeTrading,
    licenceValidityPeriod = licenceValidityPeriod,
    taxSituation = taxSituation,
    saIncomeDeclared = saIncomeDeclared,
    entityType = entityType,
    crn = crn,
    companyDetailsConfirmed = companyDetailsConfirmed,
    chargeableForCT = chargeableForCT,
    ctIncomeDeclared = ctIncomeDeclared
  )

  def completeUserAnswers(
    licenceType: LicenceType = LicenceType.DriverOfTaxisAndPrivateHires,
    licenceTimeTrading: LicenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
    licenceValidityPeriod: LicenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
    taxSituation: Option[TaxSituation] = None,
    saIncomeDeclared: Option[YesNoAnswer] = None,
    entityType: Option[EntityType] = None,
    crn: Option[CRN] = None,
    companyDetailsConfirmed: Option[YesNoAnswer] = None,
    chargeableForCT: Option[YesNoAnswer] = None,
    ctIncomeDeclared: Option[YesNoAnswer] = None
  ): CompleteUserAnswers = CompleteUserAnswers(
    licenceType = licenceType,
    licenceTimeTrading = licenceTimeTrading,
    licenceValidityPeriod = licenceValidityPeriod,
    taxSituation = taxSituation,
    saIncomeDeclared = saIncomeDeclared,
    entityType = entityType,
    crn = crn,
    companyDetailsConfirmed = companyDetailsConfirmed,
    chargeableForCT = chargeableForCT,
    ctIncomeDeclared = ctIncomeDeclared
  )

}
