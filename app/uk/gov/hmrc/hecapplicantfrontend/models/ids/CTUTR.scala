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
  def stripped: String     = CTUTR.strip(value)
  def strippedCtutr: CTUTR = CTUTR(stripped)
}

object CTUTR {

  implicit val format: Format[CTUTR] = Json.valueFormat[CTUTR]

  implicit val eq: Eq[CTUTR] = Eq.fromUniversalEquals

  def fromString(s: String): Option[CTUTR] = {
    val withoutSpaces = s.removeWhitespace
    if (CorporationTaxReferenceChecker.isValid(withoutSpaces)) Some(CTUTR(withoutSpaces)) else None
  }

  /**
    * For a correctly formatted CTUTR (10/13 digit string with/out a prefixed/suffixed 'k' or 'K')
    * - strips first/last character if it is equal to 'k' or 'K'
    * - if remaining string length = 13, strip the first 3 characters
    * Note: For an incorrectly formatted CTUTR, the string is returned as is
    * @param ctutr The CTUTR string
    * @return The stripped CTUTR if valid, the unchanged string if invalid
    */
  def strip(ctutr: String): String = {
    val k10  = """^[kK]\d{10}$"""
    val k13  = """^[kK]\d{13}$"""
    val _10k = """^\d{10}[kK]$"""
    val _13k = """^\d{13}[kK]$"""
    val _13  = """^\d{13}$"""

    if (ctutr.matches(k10)) ctutr.tail
    else if (ctutr.matches(_10k)) ctutr.init
    else if (ctutr.matches(_13)) ctutr.substring(3)
    else if (ctutr.matches(k13)) ctutr.substring(4)
    else if (ctutr.matches(_13k)) ctutr.init.substring(3)
    else ctutr
  }

}
