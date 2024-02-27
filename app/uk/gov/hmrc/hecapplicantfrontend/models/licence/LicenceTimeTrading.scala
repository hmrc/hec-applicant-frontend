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

package uk.gov.hmrc.hecapplicantfrontend.models.licence

import cats.Eq
import play.api.libs.json._

sealed trait LicenceTimeTrading extends Product with Serializable

object LicenceTimeTrading {
  case object ZeroToTwoYears extends LicenceTimeTrading

  case object TwoToFourYears extends LicenceTimeTrading

  case object FourToEightYears extends LicenceTimeTrading

  case object EightYearsOrMore extends LicenceTimeTrading

  implicit val eq: Eq[LicenceTimeTrading] = Eq.fromUniversalEquals

  implicit val format: Format[LicenceTimeTrading] = new Format[LicenceTimeTrading] {
    override def reads(json: JsValue): JsResult[LicenceTimeTrading] = json match {
      case JsString("ZeroToTwoYears")   => JsSuccess(ZeroToTwoYears)
      case JsString("TwoToFourYears")   => JsSuccess(TwoToFourYears)
      case JsString("FourToEightYears") => JsSuccess(FourToEightYears)
      case JsString("EightYearsOrMore") => JsSuccess(EightYearsOrMore)
      case _                            => JsError(s"Unknown licence time trading period: ${json.toString()}")
    }

    override def writes(o: LicenceTimeTrading): JsValue = JsString(o.toString)
  }
}
