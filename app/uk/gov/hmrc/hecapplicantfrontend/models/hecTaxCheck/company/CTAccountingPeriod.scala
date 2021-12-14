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

package uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company

import play.api.libs.json.{JsObject, JsResult, JsValue, Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.HECTaxCheckSource

import java.time.LocalDate

sealed trait CTAccountingPeriod {
  val endDate: LocalDate
  val accountingTypePeriodFor: HECTaxCheckSource
  val ctStatus: CTStatus
}

object CTAccountingPeriod {

  final case class CTAccountingPeriodDigital(
    startDate: LocalDate,
    endDate: LocalDate,
    ctStatus: CTStatus
  ) extends CTAccountingPeriod {
    val accountingTypePeriodFor = HECTaxCheckSource.Digital
  }

  implicit val format: OFormat[CTAccountingPeriod] = new OFormat[CTAccountingPeriod] {
    override def reads(json: JsValue): JsResult[CTAccountingPeriod] =
      (json \ "type")
        .validate[HECTaxCheckSource]
        .flatMap { case HECTaxCheckSource.Digital =>
          Json.reads[CTAccountingPeriodDigital].reads(json)
        }

    override def writes(accountingPeriod: CTAccountingPeriod): JsObject = {
      val json = accountingPeriod match {
        case d: CTAccountingPeriodDigital => Json.writes[CTAccountingPeriodDigital].writes(d)
      }
      json ++ Json.obj("type" -> accountingPeriod.accountingTypePeriodFor)
    }
  }

}
