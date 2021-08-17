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

import julienrf.json.derived
import play.api.libs.json.OFormat
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

sealed trait ApplicantDetails extends Product with Serializable

object ApplicantDetails {

  final case class IndividualApplicantDetails(
    ggCredId: GGCredId,
    nino: NINO,
    sautr: Option[SAUTR],
    name: Name,
    dateOfBirth: DateOfBirth
  ) extends ApplicantDetails

  final case class CompanyApplicantDetails(
    ggCredId: GGCredId,
    ctutr: CTUTR
  ) extends ApplicantDetails

  implicit val format: OFormat[ApplicantDetails] = derived.oformat()

}
