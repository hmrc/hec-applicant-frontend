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
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyHouseName, DateOfBirth, Name}

sealed trait ApplicantDetails extends Product with Serializable

object ApplicantDetails {

  final case class IndividualApplicantDetails(
    ggCredId: GGCredId,
    name: Name,
    dateOfBirth: DateOfBirth
  ) extends ApplicantDetails

  final case class CompanyApplicantDetails(
    ggCredId: GGCredId,
    crn: CRN,
    companyName: CompanyHouseName
  ) extends ApplicantDetails

  implicit val individualApplicantDetailsFormat: OFormat[IndividualApplicantDetails] = Json.format

  implicit val companyApplicantDetailsFormat: OFormat[CompanyApplicantDetails] = Json.format

}
