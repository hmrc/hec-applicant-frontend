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
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, HECSession, Name}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore

import java.time.LocalDate
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ConfirmIndividualDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore)
  )

  val controller = instanceOf[ConfirmIndividualDetailsController]

  "ConfirmIndividualDetailsController" when {

    "handling requests to the confirm individual details page" must {

      def performAction(): Future[Result] = controller.confirmIndividualDetails(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "redirect to the start endpoint" when {

        "company details are found in session" in {
          val companyRetrievedData = CompanyRetrievedData(GGCredId(""), None, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(HECSession(companyRetrievedData))
          }

          checkIsRedirect(performAction(), routes.StartController.start())
        }
      }

      "display the page" when {

        "the user is logged in and individual data can be found" in {
          val name        = Name("First", "Last")
          val dateOfBirth = DateOfBirth(LocalDate.now())

          val session = HECSession(IndividualRetrievedData(GGCredId(""), NINO(""), None, name, dateOfBirth, None))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe OK
        }

      }

    }

  }

}
