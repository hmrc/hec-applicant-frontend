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
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

sealed trait RetrievedApplicantData extends Product with Serializable {
  val entityType: EntityType
  val unexpiredTaxChecks: List[TaxCheckListItem]
}

object RetrievedApplicantData {

  final case class IndividualRetrievedData(
    loginData: IndividualLoginData,
    journeyData: IndividualJourneyData,
    unexpiredTaxChecks: List[TaxCheckListItem]
  ) extends RetrievedApplicantData {
    val entityType: EntityType = EntityType.Individual
  }

  final case class CompanyRetrievedData(
    loginData: CompanyLoginData,
    journeyData: CompanyJourneyData,
    unexpiredTaxChecks: List[TaxCheckListItem]
  ) extends RetrievedApplicantData {
    val entityType: EntityType = EntityType.Company
  }

  final case class IndividualLoginData(
    ggCredId: GGCredId,
    nino: NINO,
    sautr: Option[SAUTR],
    name: Name,
    dateOfBirth: DateOfBirth,
    emailAddress: Option[EmailAddress]
  )

  object IndividualLoginData {

    implicit val format: OFormat[IndividualLoginData] = Json.format

  }

  final case class IndividualJourneyData(
    saStatus: Option[SAStatusResponse]
  )

  object IndividualJourneyData {

    implicit val format: OFormat[IndividualJourneyData] = Json.format

    val empty: IndividualJourneyData = IndividualJourneyData(None)

  }

  final case class CompanyLoginData(
    ggCredId: GGCredId,
    ctutr: Option[CTUTR],
    emailAddress: Option[EmailAddress]
  )

  object CompanyLoginData {

    implicit val format: OFormat[CompanyLoginData] = Json.format

  }

  final case class CompanyJourneyData(
    companyName: Option[CompanyHouseName],
    desCtutr: Option[CTUTR],
    ctStatus: Option[CTStatusResponse]
  )

  object CompanyJourneyData {

    implicit val format: OFormat[CompanyJourneyData] = Json.format

    val empty: CompanyJourneyData = CompanyJourneyData(None, None, None)
  }

  implicit val format: OFormat[RetrievedApplicantData] = new OFormat[RetrievedApplicantData] {
    override def reads(json: JsValue): JsResult[RetrievedApplicantData] =
      (json \ "type")
        .validate[EntityType]
        .flatMap {
          case EntityType.Individual => Json.reads[IndividualRetrievedData].reads(json)
          case EntityType.Company    => Json.reads[CompanyRetrievedData].reads(json)
        }

    override def writes(o: RetrievedApplicantData): JsObject = {
      val json = o match {
        case i: IndividualRetrievedData => Json.writes[IndividualRetrievedData].writes(i)
        case c: CompanyRetrievedData    => Json.writes[CompanyRetrievedData].writes(c)
      }
      json ++ Json.obj("type" -> o.entityType)
    }
  }

}
