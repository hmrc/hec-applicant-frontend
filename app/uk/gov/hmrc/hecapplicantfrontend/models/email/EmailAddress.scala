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

import com.google.inject.ImplementedBy
import javax.naming.Context.{INITIAL_CONTEXT_FACTORY => ICF}
import javax.inject.Singleton
import javax.naming.directory.InitialDirContext
import scala.jdk.CollectionConverters._
import scala.util.Try

case class EmailAddress(value: String) extends StringValue {
  val (mailbox, domain): (Mailbox, Domain) = value match {
    case EmailAddressValidation.validEmail(m, d) => (Mailbox(m), Domain(d))
    case invalidEmail                            => throw new IllegalArgumentException(s"'$invalidEmail' is not a valid email address")
  }

  lazy val obfuscated: ObfuscatedEmailAddress = ObfuscatedEmailAddress.apply(value)
}

object EmailAddress {
  def isValid(email: String): Boolean =
    email match {
      case EmailAddressValidation.validEmail(_, _) => true
      case _                                       => false
    }
}

case class Mailbox(value: String) extends StringValue

case class Domain(value: String) extends StringValue {
  value match {
    case EmailAddressValidation.validDomain(_) => //
    case invalidDomain                         => throw new IllegalArgumentException(s"'$invalidDomain' is not a valid email domain")
  }
}

@ImplementedBy(classOf[EmailAddressValidation])
trait EmailValidation {
  def isValid(email: String): Boolean
}

@Singleton
class EmailAddressValidation extends EmailValidation {
  private val DNS_CONTEXT_FACTORY = "com.sun.jndi.dns.DnsContextFactory"
  private val env                 = new java.util.Hashtable[String, String]()
  env.put(ICF, DNS_CONTEXT_FACTORY)

  private def isHostMailServer(domain: String) = {
    val ictx = new InitialDirContext(env)

    def getAttributeValue(domain: String, attribute: String) =
      Try {
        ictx.getAttributes(domain, Array(attribute)).getAll.asScala.toList
      }.toEither

    getAttributeValue(domain, "MX") match {
      case Right(value) if value.nonEmpty => true
      case _                              =>
        getAttributeValue(domain, "A") match {
          case Right(value) => value.nonEmpty
          case Left(_)      => false
        }
    }
  }

  def isValid(email: String): Boolean =
    email match {
      case EmailAddressValidation.validEmail(_, _) if isHostMailServer(EmailAddress(email).domain.value) => true
      case _                                                                                             => false
    }

}

object EmailAddressValidation {
  val validEmail  = """^([a-zA-Z0-9.!#$%&''*+/=?^_`{|}~-]+)@([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r
  val validDomain = """^([a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*)$""".r
}
