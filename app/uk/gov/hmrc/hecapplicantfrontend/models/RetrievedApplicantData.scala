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
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

sealed trait RetrievedApplicantData extends Product with Serializable

object RetrievedApplicantData {

  final case class IndividualRetrievedData(
    ggCredId: GGCredId,
    nino: NINO,
    sautr: Option[SAUTR],
    name: Name,
    dateOfBirth: DateOfBirth,
    emailAddress: Option[EmailAddress],
    saStatus: Option[SAStatusResponse]
  ) extends RetrievedApplicantData

  final case class CompanyRetrievedData(
    ggCredId: GGCredId,
    ctutr: Option[CTUTR],
    emailAddress: Option[EmailAddress]
  ) extends RetrievedApplicantData

  implicit val formatIndividual: OFormat[IndividualRetrievedData] = Json.format[IndividualRetrievedData]
  implicit val formatCompany: OFormat[CompanyRetrievedData]       = Json.format[CompanyRetrievedData]

  //@SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.Equals"))
  implicit val format: OFormat[RetrievedApplicantData] = Jsonx.oFormatSealed[RetrievedApplicantData]

}
