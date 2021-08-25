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

import java.time.LocalDate

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{TimeProvider, TimeUtils}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TaxCheckCompleteControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider = mock[TimeProvider]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider)
  )

  val controller = instanceOf[TaxCheckCompleteController]

  val individualRetrievedData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None)

  "TaxCheckCompleteController" when {

    "handling request to tax situation page " must {

      def performAction(): Future[Result] = controller.taxCheckComplete(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "return an InternalServerError" when {

        "a tax check code cannot be found in session" in {
          val session = HECSession(individualRetrievedData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "display the page" when {

        "tax check code has been generated for the user " in {
          val session = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
            ),
            Some(HECTaxCheck(HECTaxCheckCode("some code"), TimeUtils.today()))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxCheckComplete.title")
          )
        }

      }

    }
  }

}