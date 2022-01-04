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

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, Error, Name}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

import java.time.LocalDate
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ConfirmIndividualDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
  )

  val controller = instanceOf[ConfirmIndividualDetailsController]

  "ConfirmIndividualDetailsController" when {

    "handling requests to the confirm individual details page" must {
      def performAction(): Future[Result] = controller.confirmIndividualDetails(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "throw an exception" when {

        "company details are found in session" in {
          val companyLoginData =
            CompanyLoginData(GGCredId(""), None, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(CompanyHECSession.newSession(companyLoginData))
          }

          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "display the page" when {

        "the user is logged in and individual data can be found" in {
          val name        = Name("First", "Last")
          val dateOfBirth = DateOfBirth(LocalDate.of(2000, 12, 3))

          val session = IndividualHECSession.newSession(
            IndividualLoginData(GGCredId(""), NINO(""), None, name, dateOfBirth, None)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
              session
            )(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmIndividualDetails.title"),
            { doc =>
              val rowValues = doc.select(".govuk-summary-list__value")
              rowValues.get(0).text shouldBe name.firstName
              rowValues.get(1).text shouldBe name.lastName
              rowValues.get(2).text shouldBe "3 December 2000"

              val link = doc.select(".govuk-body > .govuk-link")
              link.attr("href") shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit().url

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit().url

              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
            }
          )

        }

      }

    }

    "handling submits on the confirm individual details page" must {

      def performAction(): Future[Result] = controller.confirmIndividualDetailsSubmit(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "throw an exception" when {

        "company details are found in session" in {
          val companyLoginData =
            CompanyLoginData(GGCredId(""), None, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(CompanyHECSession.newSession(companyLoginData))
          }

          assertThrows[RuntimeException](await(performAction()))
        }

        "there is a problem getting the next page" in {
          val session = IndividualHECSession.newSession(
            IndividualLoginData(
              GGCredId(""),
              NINO(""),
              None,
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
              session,
              session.copy(hasConfirmedDetails = true)
            )(Left(Error("")))
          }

          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "proceed to the next page" when {

        "the next page can be found" in {
          val session = IndividualHECSession.newSession(
            IndividualLoginData(
              GGCredId(""),
              NINO(""),
              None,
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
              session,
              session.copy(hasConfirmedDetails = true)
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction(), mockNextCall)
        }

      }

    }

    "handling requests to display the confirm individual details exit page" must {

      def performAction(): Future[Result] = controller.confirmIndividualDetailsExit(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "throw an exception" when {

        "company details are found in session" in {
          val companyLoginData =
            CompanyLoginData(GGCredId(""), None, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(CompanyHECSession.newSession(companyLoginData))
          }

          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "display the page" when {

        "the user is logged in and individual data can be found" in {
          val session = IndividualHECSession.newSession(
            IndividualLoginData(
              GGCredId(""),
              NINO(""),
              None,
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit(),
              session
            )(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmIndividualDetailsExit.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              val link = doc.select(s"p > a[href=${appConfig.signOutUrl(continueUrl = None)}]")
              link.isEmpty shouldBe false

            }
          )

        }

      }

    }

  }

}
