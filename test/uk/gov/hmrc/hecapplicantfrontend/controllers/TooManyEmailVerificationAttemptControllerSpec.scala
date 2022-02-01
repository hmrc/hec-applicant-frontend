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
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.EmailAddress
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TooManyEmailVerificationAttemptControllerSpec
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

  val controller = instanceOf[TooManyEmailVerificationAttemptController]

  "TooManyEmailVerificationAttemptControllerSpec" when {

    "handling request to email address attempted too many times page" must {

      val ggEmailId = EmailAddress("user@test.com")
      val passcode  = Passcode("FFFFFF")

      def performAction(): Future[Result] = controller.tooManyEmailVerificationAttempts(FakeRequest())

      "return a technical error" when {

        "passcode Request result is other than maximum number of emails exceeded" in {
          List(
            PasscodeRequestResult.PasscodeSent.some,
            PasscodeRequestResult.EmailAddressAlreadyVerified.some,
            PasscodeRequestResult.BadEmailAddress.some
          ).foreach { passcodeRequestResult =>
            withClue(s" For passcode  Request result :: $passcodeRequestResult") {

              val session = Fixtures.individualHECSession(
                loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
                userAnswers = Fixtures.completeIndividualUserAnswers(),
                isEmailRequested = true,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = passcodeRequestResult,
                    passcode = passcode.some,
                    passcodeVerificationResult = None
                  )
                  .some
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
              }
              assertThrows[RuntimeException](await(performAction()))
            }
          }
        }

      }

      "display the page" when {
        val session = Fixtures.individualHECSession(
          loginData = Fixtures.individualLoginData(emailAddress = None),
          userAnswers = Fixtures.completeIndividualUserAnswers(),
          isEmailRequested = true,
          userEmailAnswers = Fixtures
            .userEmailAnswers(
              passcodeRequestResult = PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some
            )
            .some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("emailRequestExceeded.title"),
          { doc =>
            val htmlBody = doc.select(".govuk-body").html()

            htmlBody should include regex messageFromMessageKey(
              "emailRequestExceeded.p2",
              routes.StartController.start().url
            )

          }
        )

      }
    }

  }

}
