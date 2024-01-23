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
import play.api.libs.json._

sealed trait YesNoAnswer extends Product with Serializable

object YesNoAnswer {

  case object Yes extends YesNoAnswer

  case object No extends YesNoAnswer

  implicit val eq: Eq[YesNoAnswer] = Eq.fromUniversalEquals

  implicit val format: Format[YesNoAnswer] = new Format[YesNoAnswer] {
    override def reads(json: JsValue): JsResult[YesNoAnswer] = json match {
      case JsString("Yes") => JsSuccess(Yes)
      case JsString("No")  => JsSuccess(No)
      case _               => JsError(s"Unknown yes/no answer: ${json.toString()}")
    }

    override def writes(o: YesNoAnswer): JsValue = JsString(o.toString)
  }

  val values: List[YesNoAnswer] = List(Yes, No)

}
