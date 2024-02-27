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

package uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company

import cats.Eq
import play.api.libs.json._

sealed trait CTStatus extends Product with Serializable

object CTStatus {

  case object ReturnFound extends CTStatus

  case object NoticeToFileIssued extends CTStatus

  case object NoReturnFound extends CTStatus

  implicit val eq: Eq[CTStatus] = Eq.fromUniversalEquals

  implicit val format: Format[CTStatus] = new Format[CTStatus] {
    override def reads(json: JsValue): JsResult[CTStatus] = json match {
      case JsString("ReturnFound")        => JsSuccess(ReturnFound)
      case JsString("NoticeToFileIssued") => JsSuccess(NoticeToFileIssued)
      case JsString("NoReturnFound")      => JsSuccess(NoReturnFound)
      case _                              => JsError(s"Unknown CT status: ${json.toString()}")
    }

    override def writes(o: CTStatus): JsValue = JsString(o.toString)
  }

}
