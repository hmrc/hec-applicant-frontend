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

package uk.gov.hmrc.hecapplicantfrontend.models

import ai.x.play.json.Jsonx
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.ApplicantDetails.{CompanyApplicantDetails, IndividualApplicantDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDetails.{CompanyTaxDetails, IndividualTaxDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceDetails

sealed trait HECTaxCheckData extends Product with Serializable

object HECTaxCheckData {

  final case class IndividualHECTaxCheckData(
    applicantDetails: IndividualApplicantDetails,
    licenceDetails: LicenceDetails,
    taxDetails: IndividualTaxDetails
  ) extends HECTaxCheckData

  final case class CompanyHECTaxCheckData(
    applicantDetails: CompanyApplicantDetails,
    licenceDetails: LicenceDetails,
    taxDetails: CompanyTaxDetails
  ) extends HECTaxCheckData

  implicit val formatIndividual: OFormat[IndividualHECTaxCheckData] = Json.format[IndividualHECTaxCheckData]
  implicit val formatCompany: OFormat[CompanyHECTaxCheckData]       = Json.format[CompanyHECTaxCheckData]
  implicit val format: OFormat[HECTaxCheckData]                     = Jsonx.oFormatSealed[HECTaxCheckData]

}
