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

package uk.gov.hmrc.hecapplicantfrontend.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Language}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequest, PasscodeVerificationRequest}
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class EmailVerificationConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec {
  val (protocol, host, port) = ("http", "host", "123")

  val config                     = Configuration(
    ConfigFactory.parseString(s"""
                                 | microservice.services.email-verification {
                                 |    protocol = "$protocol"
                                 |    host     = "$host"
                                 |    port     = $port
                                 |  }
                                 |""".stripMargin)
  )
  val connector                  = new EmailVerificationConnectorImpl(mockHttp, new ServicesConfig(config))
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "EmailVerificationConnectorSpec" when {

    "handling request to requestPasscode" must {

      val passcodeRequest = PasscodeRequest(EmailAddress("user@test.com"), "hec", Language.English)

      val expectedUrl = url"$protocol://$host:$port/email-verification/request-passcode"

      behave like connectorBehaviour(
        mockPost(expectedUrl, passcodeRequest)(_),
        () => connector.requestPasscode(passcodeRequest)
      )

    }

    "handling request to verifyPasscode" must {

      val passcodeVerificationRequest = PasscodeVerificationRequest(Passcode("AA12345"), EmailAddress("user@test.com"))

      val expectedUrl = url"$protocol://$host:$port/email-verification/verify-passcode"

      behave like connectorBehaviour(
        mockPost(expectedUrl, passcodeVerificationRequest)(_),
        () => connector.verifyPasscode(passcodeVerificationRequest)
      )

    }
  }
}
