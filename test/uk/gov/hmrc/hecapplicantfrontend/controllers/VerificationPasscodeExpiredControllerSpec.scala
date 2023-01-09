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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.implicits.catsSyntaxOptionId
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.EmailAddress
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class VerificationPasscodeExpiredControllerSpec
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

  val controller = instanceOf[VerificationPasscodeExpiredController]

  "VerificationPasscodeExpiredControllerSec" when {

    "handling request to verification passcode expired page" must {
      val ggEmailId       = EmailAddress("user@test.com")
      val expiredPasscode = Passcode("FFFFFF")

      def performAction(): Future[Result] = controller.verificationPasscodeExpired(FakeRequest())

      def getSession(passcodeVerificationResult: Option[PasscodeVerificationResult]) = Fixtures.individualHECSession(
        loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
        emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
        userEmailAnswers = Fixtures
          .userEmailAnswers(
            passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
            passcode = expiredPasscode.some,
            passcodeVerificationResult = passcodeVerificationResult
          )
          .some
      )

      "return a technical error" when {

        "passcode verification result is other than Expired" in {

          List(
            PasscodeVerificationResult.Match.some,
            PasscodeVerificationResult.NoMatch.some,
            PasscodeVerificationResult.TooManyAttempts.some
          ).foreach { passcodeVerificationResult =>
            withClue(s" For passcode  verification result :: $passcodeVerificationResult") {
              val session = getSession(passcodeVerificationResult)
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }
              assertThrows[InconsistentSessionState](await(performAction()))
            }
          }

        }

      }

      "display the page" in {

        val session = getSession(PasscodeVerificationResult.Expired.some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("verifyPasscodeExpired.title"),
          { doc =>
            val htmlBody = doc.select(".govuk-body").html()

            htmlBody should include regex messageFromMessageKey(
              "verifyPasscodeExpired.p2",
              routes.ResendEmailConfirmationController.resendEmail.url,
              routes.StartController.start.url
            )

          }
        )

      }
    }
  }
}
