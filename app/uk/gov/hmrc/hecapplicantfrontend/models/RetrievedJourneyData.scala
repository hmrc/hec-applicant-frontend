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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.json._
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTStatusResponse
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.SAStatusResponse
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CTUTR

sealed trait RetrievedJourneyData extends Product with Serializable

object RetrievedJourneyData {

  final case class IndividualRetrievedJourneyData(saStatus: Option[SAStatusResponse]) extends RetrievedJourneyData

  object IndividualRetrievedJourneyData {

    val empty: IndividualRetrievedJourneyData = IndividualRetrievedJourneyData(None)

    implicit val format: OFormat[IndividualRetrievedJourneyData] = Json.format

  }

  final case class CompanyRetrievedJourneyData(
    companyName: Option[CompanyHouseName],
    desCtutr: Option[CTUTR],
    ctStatus: Option[CTStatusResponse]
  ) extends RetrievedJourneyData

  object CompanyRetrievedJourneyData {

    val empty: CompanyRetrievedJourneyData = CompanyRetrievedJourneyData(None, None, None)

    implicit val format: OFormat[CompanyRetrievedJourneyData] = Json.format

  }

}
