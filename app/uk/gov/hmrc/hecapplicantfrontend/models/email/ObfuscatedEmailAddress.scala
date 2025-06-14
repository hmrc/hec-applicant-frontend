/*
 * Copyright 2025 HM Revenue & Customs
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

package uk.gov.hmrc.hecapplicantfrontend.models.email

import scala.util.matching.Regex

trait ObfuscatedEmailAddress extends StringValue

object ObfuscatedEmailAddress {
  final private val shortMailbox: Regex = "(.{1,2})".r
  final private val longMailbox: Regex  = "(.)(.*)(.)".r

  implicit def obfuscatedEmailToString(e: ObfuscatedEmailAddress): String = e.value

  def apply(plainEmailAddress: String): ObfuscatedEmailAddress =
    new ObfuscatedEmailAddress {
      val value: String = plainEmailAddress match {
        case EmailAddressValidation.validEmail(shortMailbox(m), domain)                              =>
          s"${obscure(m)}@$domain"
        case EmailAddressValidation.validEmail(longMailbox(firstLetter, middle, lastLetter), domain) =>
          s"$firstLetter${obscure(middle)}$lastLetter@$domain"
        case invalidEmail                                                                            =>
          throw new IllegalArgumentException(s"Cannot obfuscate invalid email address '$invalidEmail'")
      }
    }

  def apply(email: EmailAddress): ObfuscatedEmailAddress = apply(email.value)

  private def obscure(text: String): String = "*" * text.length
}
