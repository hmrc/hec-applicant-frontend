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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import org.jsoup.nodes.Element
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.views.LicenceTypeOption
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class TaxChecksListControllerSpec
    extends ControllerSpec
    with JourneyServiceSupport
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
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
          IndividualHECSession(
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

        status(performAction()) shouldBe INTERNAL_SERVER_ERROR
      }

      "display the page with tax checks sorted by create date" in {
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
        val session           = IndividualHECSession(
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
          mockJourneyServiceGetPrevious(routes.TaxChecksListController.unexpiredTaxChecks(), session)(mockPreviousCall)
        }

        def verifyTaxCheckListItem(element: Element, item: TaxCheckListItem) = {
          val licenceTypeMessageKey = s"licenceType.${LicenceTypeOption.licenceTypeOption(item.licenceType).messageKey}"
          element.select(".govuk-heading-m").text() shouldBe messageFromMessageKey(licenceTypeMessageKey)

          val captions = element.select(".govuk-caption-m")
          captions.size() shouldBe 2

          val expiryCaption = captions.get(0)
          expiryCaption.text() shouldBe s"${messageFromMessageKey("taxCheck.expiryKey")} 10 January 2020"

          val taxCheckCodeCaption = captions.get(1)
          taxCheckCodeCaption
            .text() shouldBe s"${messageFromMessageKey("taxCheck.codeKey")} ${item.taxCheckCode.value.grouped(3).mkString(" ")}"

          element.select("button").text() should include regex item.taxCheckCode.value
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("taxChecksList.title"),
          doc => {
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val taxChecks = doc.select(".existing-code")
            taxChecks.size() shouldBe 3
            verifyTaxCheckListItem(taxChecks.get(0), todayTaxCheck)
            verifyTaxCheckListItem(taxChecks.get(1), yesterdayTaxCheck)
            verifyTaxCheckListItem(taxChecks.get(2), dayBeforeTaxCheck)

            doc.select("form").select("button").text() shouldBe messageFromMessageKey("taxChecksList.button")
          }
        )

      }

    }
  }

}
