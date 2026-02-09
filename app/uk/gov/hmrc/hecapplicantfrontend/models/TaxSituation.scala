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

import cats.Eq
import play.api.libs.json.*

sealed trait TaxSituation extends Product with Serializable

object TaxSituation {

  case object PAYE extends TaxSituation

  case object SA extends TaxSituation

  case object SAPAYE extends TaxSituation

  case object NotChargeable extends TaxSituation

  implicit val eq: Eq[TaxSituation] = Eq.fromUniversalEquals

  implicit val format: Format[TaxSituation] = new Format[TaxSituation] {
    override def reads(json: JsValue): JsResult[TaxSituation] = json match {
      case JsString("PAYE")          => JsSuccess(PAYE)
      case JsString("SA")            => JsSuccess(SA)
      case JsString("SAPAYE")        => JsSuccess(SAPAYE)
      case JsString("NotChargeable") => JsSuccess(NotChargeable)
      case _                         => JsError(s"Unknown tax situation: ${json.toString()}")
    }

    override def writes(o: TaxSituation): JsValue = JsString(o.toString)
  }
}
