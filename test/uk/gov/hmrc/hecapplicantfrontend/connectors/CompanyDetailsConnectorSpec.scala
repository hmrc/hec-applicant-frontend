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
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.ExecutionContext.Implicits.global

class CompanyDetailsConnectorSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with HttpSupport
    with ConnectorSpec {

  val (protocol, host, port) = ("http", "host", "123")

  val config    = Configuration(
    ConfigFactory.parseString(s"""
                                 | microservice.services.companies-house-proxy {
                                 |    protocol = "$protocol"
                                 |    host     = "$host"
                                 |    port     = $port
                                 |  }
                                 |""".stripMargin)
  )
  val connector = new CompanyDetailsConnectorImpl(mockHttp, new ServicesConfig(config))

  "CompanyDetailsConnectorImpl" when {

    "handling requests to get company details" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val companyNumber = CRN("1234567")

      val expectedUrl = s"$protocol://$host:$port/companies-house-api-proxy/company/1234567"

      behave like connectorBehaviour(
        mockGet[HttpResponse](expectedUrl)(_),
        () => connector.findCompany(companyNumber)
      )

    }

  }

}
