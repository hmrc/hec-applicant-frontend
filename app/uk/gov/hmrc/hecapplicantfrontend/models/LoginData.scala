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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}

sealed trait LoginData extends Product with Serializable {

  val ggCredId: GGCredId

  val didConfirmUncertainEntityType: Option[Boolean]

}

object LoginData {

  final case class IndividualLoginData(
    ggCredId: GGCredId,
    nino: NINO,
    sautr: Option[SAUTR],
    name: Name,
    dateOfBirth: DateOfBirth,
    emailAddress: Option[EmailAddress],
    didConfirmUncertainEntityType: Option[Boolean]
  ) extends LoginData

  object IndividualLoginData {

    implicit val format: OFormat[IndividualLoginData] = Json.format

  }

  final case class CompanyLoginData(
    ggCredId: GGCredId,
    ctutr: Option[CTUTR],
    emailAddress: Option[EmailAddress],
    didConfirmUncertainEntityType: Option[Boolean]
  ) extends LoginData

  object CompanyLoginData {

    implicit val format: OFormat[CompanyLoginData] = Json.format

  }

}
