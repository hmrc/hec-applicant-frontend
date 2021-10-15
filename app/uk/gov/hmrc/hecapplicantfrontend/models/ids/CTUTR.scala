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

package uk.gov.hmrc.hecapplicantfrontend.models.ids

import cats.Eq
import play.api.libs.json.{Format, Json}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.referencechecker.CorporationTaxReferenceChecker

/**
  * Corporation Tax Unique Taxpayer Reference number
  */
final case class CTUTR(value: String) extends AnyVal {
  def stripped: String = CTUTR.strip(value)
}

object CTUTR {

  implicit val format: Format[CTUTR] = Json.valueFormat[CTUTR]

  implicit val eq: Eq[CTUTR] = Eq.fromUniversalEquals

  def fromString(s: String): Option[CTUTR] = {
    val withoutSpaces = s.removeWhitespace
    if (CorporationTaxReferenceChecker.isValid(withoutSpaces)) Some(CTUTR(withoutSpaces)) else None
  }

  def strip(validCtutr: String): String = {
    val len         = validCtutr.length
    val startsWithK = validCtutr.startsWith("k")
    val endsWithK   = validCtutr.endsWith("k")

    len match {
      case 10                => validCtutr
      case 13                => validCtutr.substring(3)
      case 11 if startsWithK => validCtutr.tail
      case 11 if endsWithK   => validCtutr.init
      case 14 if startsWithK => validCtutr.substring(4)
      case 14 if endsWithK   => validCtutr.init.substring(3)
      case _                 => sys.error("Invalid CTUTR")
    }
  }

}
