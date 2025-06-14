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
import org.scalatest.matchers.should._
import org.scalatest.wordspec._
import play.api.Configuration
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global

class IvConnectorImplSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  val config = Configuration(
    ConfigFactory.parseString(
      """
        |microservice {
        |  services {
        |    iv {
        |      protocol = http
        |      host     = host
        |      port     = 123
        |    }
        |  }
        |}
        |""".stripMargin
    )
  )

  val connector = new IvConnectorImpl(
    mockHttp,
    new ServicesConfig(config)
  )

  "IvConnectorImpl" when {

    "handling requests to get a failed journey status" must {
      implicit val hc: HeaderCarrier = HeaderCarrier()
      val journeyId                  = UUID.randomUUID()
      val expectedUrl                =
        url"http://host:123/mdtp/journey/journeyId/${journeyId.toString}"

      behave like connectorBehaviour(
        mockGet(expectedUrl),
        () => connector.getFailedJourneyStatus(journeyId)
      )
    }
  }
}
