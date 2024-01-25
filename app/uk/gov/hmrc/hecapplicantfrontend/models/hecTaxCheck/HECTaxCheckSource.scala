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

import play.api.libs.json._

sealed trait HECTaxCheckSource extends Product with Serializable

object HECTaxCheckSource {

  case object Digital extends HECTaxCheckSource

  implicit val format: Format[HECTaxCheckSource] = new Format[HECTaxCheckSource] {
    override def reads(json: JsValue): JsResult[HECTaxCheckSource] = json match {
      case JsString("Digital") => JsSuccess(Digital)
      case _                   => JsError(s"Unknown HEC tax check source: ${json.toString()}")
    }

    override def writes(o: HECTaxCheckSource): JsValue = JsString(o.toString)
  }

}
