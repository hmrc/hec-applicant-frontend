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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import org.jsoup.nodes.Element
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckCodesDisplayed
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.views.LicenceTypeOption
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, AuditServiceSupport, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class TaxChecksListControllerSpec
    extends ControllerSpec
    with JourneyServiceSupport
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with AuditServiceSupport {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[AuditService].toInstance(mockAuditService)
  )

  val controller = instanceOf[TaxChecksListController]

  val individualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  "TaxChecksListController" when {

    "handling requests to display the tax check codes page" must {

      def performAction() = controller.unexpiredTaxChecks(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      val answers = Fixtures.incompleteIndividualUserAnswers(
        Some(LicenceType.ScrapMetalMobileCollector)
      )

      "return an error when no tax checks found" in {
        val session =
          Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            answers,
            None,
            None,
            List.empty
          )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        assertThrows[RuntimeException](await(performAction()))

      }

      "display the page with tax checks grouped by licence type" in {
        val expiryDate        = LocalDate.of(2020, 1, 10)
        val today             = ZonedDateTime.now()
        val yesterday         = today.minusDays(1)
        val dayBefore         = today.minusDays(2)
        val todayTaxCheck     = TaxCheckListItem(
          LicenceType.ScrapMetalMobileCollector,
          HECTaxCheckCode("2DYFK48KL"),
          expiryDate,
          today
        )
        val dayBeforeTaxCheck = TaxCheckListItem(
          LicenceType.DriverOfTaxisAndPrivateHires,
          HECTaxCheckCode("XRCYRKA74"),
          expiryDate,
          dayBefore
        )
        val yesterdayTaxCheck = TaxCheckListItem(
          LicenceType.ScrapMetalMobileCollector,
          HECTaxCheckCode("THP3T2TXL"),
          expiryDate,
          yesterday
        )
        val unsortedTaxChecks = List(dayBeforeTaxCheck, todayTaxCheck, yesterdayTaxCheck)
        val session           = Fixtures.individualHECSession(
          individualLoginData,
          IndividualRetrievedJourneyData.empty,
          answers,
          None,
          None,
          unsortedTaxChecks
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockSendAuditEvent(
            TaxCheckCodesDisplayed(individualLoginData.ggCredId, unsortedTaxChecks.map(_.taxCheckCode))
          )
        }

        def verifyLicenceGroup(
          element: Element,
          licenceType: LicenceType,
          expectedTaxChecks: List[TaxCheckListItem]
        ) = {
          val heading = element.select("h2")
          heading
            .text() shouldBe s"${messageFromMessageKey(s"licenceType.${LicenceTypeOption.licenceTypeOption(licenceType).messageKey}")}"
          val taxCheckItems = element.select(".existing-code")
          taxCheckItems.size() shouldBe expectedTaxChecks.size
          expectedTaxChecks.zipWithIndex.map { case (t, index) =>
            verifyTaxCheckListItem(taxCheckItems.get(index), t)
          }
        }

        def verifyTaxCheckListItem(element: Element, item: TaxCheckListItem) = {
          val keys = element.select(".govuk-summary-list__key")
          keys.size() shouldBe 2

          val expiryKey = keys.get(0)
          expiryKey.text() shouldBe s"${messageFromMessageKey("taxCheck.expiryKey")}"

          val taxCheckCodeKey = keys.get(1)
          taxCheckCodeKey
            .text() shouldBe s"${messageFromMessageKey("taxCheck.codeKey")}"

          val values = element.select(".govuk-summary-list__value")
          values.size() shouldBe 2

          val expiryValue = values.get(0)
          expiryValue.text() shouldBe TimeUtils.govDisplayFormat(item.expiresAfter)

          val taxCheckCodeValue = values.get(1)
          taxCheckCodeValue.text() shouldBe item.taxCheckCode.value.grouped(3).mkString(" ")

          val copyButton = element.select("button")
          copyButton.select(".copy-content").text()          shouldBe s"${messageFromMessageKey("button.copy")}"
          copyButton.select(".copied-content").text()        shouldBe s"${messageFromMessageKey("button.copied")}"
          copyButton.select(".govuk-visually-hidden").text() shouldBe s"${messageFromMessageKey(
            "taxChecksList.copyButtonScreenReaderText",
            messageFromMessageKey(s"licenceType.${LicenceTypeOption.licenceTypeOption(item.licenceType).messageKey}"),
            item.taxCheckCode.value
          )}"
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("taxChecksList.title"),
          doc => {
            val licenceGroups = doc.select(".licence-type-group")
            licenceGroups.size()                       shouldBe 2
            verifyLicenceGroup(
              licenceGroups.get(0),
              LicenceType.ScrapMetalMobileCollector,
              List(todayTaxCheck, yesterdayTaxCheck)
            )
            verifyLicenceGroup(licenceGroups.get(1), LicenceType.DriverOfTaxisAndPrivateHires, List(dayBeforeTaxCheck))
            doc.select("form").select("button").text() shouldBe messageFromMessageKey(
              "taxChecksList.button"
            )
          }
        )

      }

    }
  }

}
