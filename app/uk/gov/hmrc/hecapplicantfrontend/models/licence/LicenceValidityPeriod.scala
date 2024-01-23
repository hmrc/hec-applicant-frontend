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

sealed abstract class LicenceValidityPeriod(val licensePeriod: String) extends Product with Serializable

object LicenceValidityPeriod {

  case object UpToOneYear extends LicenceValidityPeriod("UpToOneYear")
  case object UpToTwoYears extends LicenceValidityPeriod("UpToTwoYears")
  case object UpToThreeYears extends LicenceValidityPeriod("UpToThreeYears")
  case object UpToFourYears extends LicenceValidityPeriod("UpToFourYears")
  case object UpToFiveYears extends LicenceValidityPeriod("UpToFiveYears")

  implicit val eq: Eq[LicenceValidityPeriod] = Eq.fromUniversalEquals

  implicit val format: Format[LicenceValidityPeriod] = new Format[LicenceValidityPeriod] {
    override def writes(o: LicenceValidityPeriod): JsValue = JsString(o.licensePeriod)

    override def reads(json: JsValue): JsResult[LicenceValidityPeriod] = json match {
      case JsString("UpToOneYear")    => JsSuccess(UpToOneYear)
      case JsString("UpToTwoYears")   => JsSuccess(UpToTwoYears)
      case JsString("UpToThreeYears") => JsSuccess(UpToThreeYears)
      case JsString("UpToFourYears")  => JsSuccess(UpToFourYears)
      case JsString("UpToFiveYears")  => JsSuccess(UpToFiveYears)
      case _                          => JsError(s"Unknown validity period: ${json.toString()}")
    }
  }

}
