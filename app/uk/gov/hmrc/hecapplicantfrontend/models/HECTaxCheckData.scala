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

import play.api.libs.json._
import uk.gov.hmrc.hecapplicantfrontend.models.ApplicantDetails.{CompanyApplicantDetails, IndividualApplicantDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDetails.{CompanyTaxDetails, IndividualTaxDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceDetails

sealed trait HECTaxCheckData extends Product with Serializable {
  val entityType: EntityType
}

object HECTaxCheckData {

  final case class IndividualHECTaxCheckData(
    applicantDetails: IndividualApplicantDetails,
    licenceDetails: LicenceDetails,
    taxDetails: IndividualTaxDetails
  ) extends HECTaxCheckData {
    val entityType: EntityType = EntityType.Individual
  }

  final case class CompanyHECTaxCheckData(
    applicantDetails: CompanyApplicantDetails,
    licenceDetails: LicenceDetails,
    taxDetails: CompanyTaxDetails
  ) extends HECTaxCheckData {
    val entityType: EntityType = EntityType.Company
  }

  implicit val format: OFormat[HECTaxCheckData] = new OFormat[HECTaxCheckData] {

    override def reads(json: JsValue): JsResult[HECTaxCheckData] =
      (json \ "type")
        .validate[EntityType]
        .flatMap {
          case EntityType.Individual => Json.reads[IndividualHECTaxCheckData].reads(json)
          case EntityType.Company    => Json.reads[CompanyHECTaxCheckData].reads(json)
        }

    override def writes(o: HECTaxCheckData): JsObject = {
      val json = o match {
        case i: IndividualHECTaxCheckData => Json.writes[IndividualHECTaxCheckData].writes(i)
        case c: CompanyHECTaxCheckData    => Json.writes[CompanyHECTaxCheckData].writes(c)
      }
      json ++ Json.obj("type" -> o.entityType)
    }
  }

}
