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

package uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTStatusResponse
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.SAStatusResponse

sealed trait TaxDetails extends Product with Serializable

object TaxDetails {

  final case class IndividualTaxDetails(
    nino: NINO,
    sautr: Option[SAUTR],
    taxSituation: TaxSituation,
    saIncomeDeclared: Option[YesNoAnswer],
    saStatusResponse: Option[SAStatusResponse],
    relevantIncomeTaxYear: TaxYear
  ) extends TaxDetails

  final case class CompanyTaxDetails(
    hmrcCTUTR: CTUTR,
    userSuppliedCTUTR: Option[CTUTR],
    ctIncomeDeclared: Option[YesNoAnswer],
    ctStatus: CTStatusResponse,
    recentlyStaredTrading: Option[YesNoAnswer],
    chargeableForCT: Option[YesNoAnswer]
  ) extends TaxDetails

  implicit val individualTaxDetailsFormat: OFormat[IndividualTaxDetails] = Json.format

  implicit val companyTaxDetailsFormat: OFormat[CompanyTaxDetails] = Json.format

}
