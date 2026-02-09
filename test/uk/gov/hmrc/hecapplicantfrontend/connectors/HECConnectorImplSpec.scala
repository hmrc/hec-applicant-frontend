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
import uk.gov.hmrc.hecapplicantfrontend.models.*
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.ApplicantDetails.IndividualApplicantDetails
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.HECTaxCheckData.IndividualHECTaxCheckData
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.TaxDetails.IndividualTaxDetails
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.{HECTaxCheckData, HECTaxCheckSource, SaveEmailAddressRequest}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.*
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceDetails, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.http.{HeaderCarrier, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class HECConnectorImplSpec extends AnyWordSpec with Matchers with MockFactory with HttpSupport with ConnectorSpec {

  val (protocol, host, port) = ("http", "host", "123")

  val config = Configuration(
    ConfigFactory.parseString(s"""
                                 | microservice.services.hec {
                                 |    protocol = "$protocol"
                                 |    host     = "$host"
                                 |    port     = $port
                                 |  }
                                 |""".stripMargin)
  )

  val connector                  = new HECConnectorImpl(mockHttp, new ServicesConfig(config))
  implicit val hc: HeaderCarrier = HeaderCarrier()

  "HECConnectorImpl" when {

    "handling requests to save tax checks" must {

      val taxCheckStartDateTime = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))

      val individualTaxCheckData: HECTaxCheckData =
        IndividualHECTaxCheckData(
          IndividualApplicantDetails(
            GGCredId(""),
            Name("", ""),
            DateOfBirth(LocalDate.now())
          ),
          LicenceDetails(
            LicenceType.ScrapMetalMobileCollector,
            LicenceTimeTrading.EightYearsOrMore,
            LicenceValidityPeriod.UpToThreeYears
          ),
          IndividualTaxDetails(
            NINO(""),
            Some(SAUTR("")),
            TaxSituation.SA,
            Some(YesNoAnswer.Yes),
            None,
            TaxYear(2021)
          ),
          taxCheckStartDateTime,
          HECTaxCheckSource.Digital,
          Language.English,
          Some(true),
          Some(false)
        )

      val expectedUrl = url"$protocol://$host:$port/hec/tax-check"

      behave like connectorBehaviour(
        mockPost(expectedUrl, individualTaxCheckData)(_),
        () => connector.saveTaxCheck(individualTaxCheckData)
      )

    }

    "handling requests to get SA statuses" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val sautr = SAUTR("1234567890")

      val taxYear = TaxYear(2020)

      val expectedUrl = url"$protocol://$host:$port/hec/sa-status/${sautr.value}/2020"

      behave like connectorBehaviour(
        mockGet(expectedUrl)(_),
        () => connector.getSAStatus(sautr, taxYear)
      )

    }

    "handling requests to get CT statuses" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val ctutr = CTUTR("1234567890")

      val startDate = LocalDate.of(2020, 12, 31)

      val endDate = LocalDate.of(2021, 6, 20)

      val expectedUrl = url"$protocol://$host:$port/hec/ct-status/1234567890/2020-12-31/2021-06-20"

      behave like connectorBehaviour(
        mockGet(expectedUrl)(_),
        () => connector.getCTStatus(ctutr, startDate, endDate)
      )

    }

    "handling requests to get CTUTR from CRN" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val crn = CRN("AA12345")

      val expectedUrl = url"$protocol://$host:$port/hec/ctutr/${crn.value}"

      behave like connectorBehaviour(
        mockGet(expectedUrl),
        () => connector.getCtutr(crn)
      )

    }

    "handling requests to get unexpired tax checks" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val expectedUrl = url"$protocol://$host:$port/hec/unexpired-tax-checks"

      behave like connectorBehaviour(
        mockGet(expectedUrl),
        () => connector.getUnexpiredTaxCheckCodes()
      )

    }

    "handling requests to save email addresses" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val saveEmailAddressRequest = SaveEmailAddressRequest(EmailAddress("email"), HECTaxCheckCode("code"))

      val expectedUrl = url"$protocol://$host:$port/hec/email-address"

      behave like connectorBehaviour(
        mockPost(expectedUrl, saveEmailAddressRequest)(_),
        () => connector.saveEmailAddress(saveEmailAddressRequest)
      )

    }

  }

}
