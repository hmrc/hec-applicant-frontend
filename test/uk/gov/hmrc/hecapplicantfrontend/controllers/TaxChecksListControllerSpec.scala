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

import play.api.inject.bind
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.IndividualRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.IncompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

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

  val individualRetrievedData =
    IndividualRetrievedData(
      GGCredId(""),
      NINO(""),
      None,
      Name("", ""),
      DateOfBirth(LocalDate.now()),
      None,
      None,
      List.empty
    )

  "TaxChecksListController" when {

    "handling requests to display the tax check codes page" must {

      def performAction() = controller.unexpiredTaxChecks(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "display the page with tax checks sorted by date" in {
        val answers = IncompleteUserAnswers(
          Some(LicenceType.ScrapMetalMobileCollector),
          None,
          None,
          None,
          None,
          None,
          None
        )

        val expiryDate        = LocalDate.now()
        val today             = ZonedDateTime.now()
        val yesterday         = today.minusDays(1)
        val tomorrow          = today.plusDays(1)
        val unsortedTaxChecks = List(
          TaxCheckListItem(
            LicenceType.DriverOfTaxisAndPrivateHires,
            HECTaxCheckCode("code1"),
            expiryDate,
            tomorrow
          ),
          TaxCheckListItem(
            LicenceType.ScrapMetalMobileCollector,
            HECTaxCheckCode("code2"),
            expiryDate,
            today
          ),
          TaxCheckListItem(
            LicenceType.ScrapMetalMobileCollector,
            HECTaxCheckCode("code3"),
            expiryDate,
            yesterday
          )
        )
        val session           = HECSession(individualRetrievedData.copy(unexpiredTaxChecks = unsortedTaxChecks), answers, None)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.TaxChecksListController.unexpiredTaxChecks(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("taxChecksList.title"),
          // TODO check that the tax checks are displayed and are sorted
          doc => doc.select("#back").attr("href") shouldBe mockPreviousCall.url
        )

      }

    }
  }

}
