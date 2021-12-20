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

import cats.implicits.catsSyntaxOptionId

import java.time.{LocalDate, ZonedDateTime}
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TaxCheckCompleteControllerSpec
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

  val controller = instanceOf[TaxCheckCompleteController]

  val individualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData =
    CompanyLoginData(GGCredId(""), None, None)

  "TaxCheckCompleteController" when {

    "handling request to tax situation page " must {

      def performAction(): Future[Result] = controller.taxCheckComplete(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "return a technical error" when {

        "a tax check code cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "display the page" when {

        "tax check code has been generated for the user " in {
          val taxCheckCode = "LXB7G6DX7"
          val expiryDate   = LocalDate.of(2020, 1, 8)
          val session      = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)),
            Some(HECTaxCheck(HECTaxCheckCode(taxCheckCode), expiryDate))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxCheckComplete.title"),
            doc => {
              doc.select(".govuk-panel__body").text should include regex "LXB 7G6 DX7"
              val list = doc.select(".govuk-list--number").html
              list                           should include regex messageFromMessageKey(
                "taxCheckComplete.list2",
                routes.TaxCheckCompleteController.emailTaxCheckCode().url
              )
              list                           should include regex messageFromMessageKey(
                "taxCheckComplete.list3",
                routes.StartController.start().url
              )
              doc.select(".govuk-body").html should include regex messageFromMessageKey(
                "taxCheckComplete.p2",
                "8 January 2020"
              )

            }
          )
        }

      }

    }

    "handling request to show email options after tax check code has been generated" must {

      def performAction(): Future[Result] =
        controller.emailTaxCheckCode(FakeRequest())

      val now = ZonedDateTime.now()

      "return a technical error" when {

        "the call to update and next fails" in {
          val session        = Fixtures.individualHECSession()
          val updatedSession = session.copy(isEmailRequested = true)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.TaxCheckCompleteController.taxCheckComplete(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "redirect to the next page" when {

        def nextPageRedirectTest(
          session: HECSession,
          updatedSession: HECSession
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.TaxCheckCompleteController.taxCheckComplete(),
              session,
              updatedSession
            )(
              Right(mockNextCall)
            )
          }

          checkIsRedirect(performAction(), mockNextCall)
        }

        "valid Individual tax check code has been generated" in {
          val answers = Fixtures.completeIndividualUserAnswers(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            TaxSituation.SA,
            Some(YesNoAnswer.Yes),
            Some(EntityType.Individual)
          )
          val session =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              taxCheckStartDateTime = Some(now)
            )
          nextPageRedirectTest(session, session.copy(isEmailRequested = true))
        }

        "valid Company data is submitted and" in {
          val answers = Fixtures.completeCompanyUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            recentlyStartedTrading = YesNoAnswer.Yes.some
          )
          val session = Fixtures.companyHECSession(
            companyLoginData,
            CompanyRetrievedJourneyData.empty,
            answers,
            taxCheckStartDateTime = Some(now)
          )
          nextPageRedirectTest(session, session.copy(isEmailRequested = true))
        }

      }

    }
  }

}
