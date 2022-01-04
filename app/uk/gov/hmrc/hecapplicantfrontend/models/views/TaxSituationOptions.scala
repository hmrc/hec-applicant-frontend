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

package uk.gov.hmrc.hecapplicantfrontend.models.views

import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation
import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation._

final case class TaxSituationOption(messageKey: String, hintKey: Option[String])
object TaxSituationOption {

  def taxSituationOption(taxSituation: TaxSituation): TaxSituationOption =
    taxSituation match {
      case PAYE          => TaxSituationOption("PA", None)
      case SA            => TaxSituationOption("SA", None)
      case SAPAYE        => TaxSituationOption("SAPAYE", None)
      case NotChargeable => TaxSituationOption("NotChargeable", Some("NotChargeable.hint"))
    }

}
