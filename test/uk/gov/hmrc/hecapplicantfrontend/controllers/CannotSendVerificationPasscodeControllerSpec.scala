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
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CannotSendVerificationPasscodeControllerSpec
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

  val controller = instanceOf[CannotSendVerificationPasscodeController]

  "CannotSendVerificationPasscodeControllerSpec" when {

    "handling request too cannot send verification passcode email " must {
      val ggEmailId = EmailAddress("user@test.com")

      def performAction(): Future[Result] = controller.cannotSendVerificationPasscode(FakeRequest())

      "return a technical error" when {

        "passcode request result is other than bad email request" in {

          List(
            PasscodeRequestResult.PasscodeSent.some,
            PasscodeRequestResult.EmailAddressAlreadyVerified.some,
            PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some
          ).foreach { passcodeRequestResult =>
            withClue(s" For passcode  request result :: $passcodeRequestResult") {

              val session = Fixtures.individualHECSession(
                loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
                userAnswers = Fixtures.completeIndividualUserAnswers(),
                isEmailRequested = true,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = passcodeRequestResult
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

        def test(emailAddress: Option[EmailAddress]) = {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = emailAddress),
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = Fixtures
              .userEmailAnswers(
                passcodeRequestResult = PasscodeRequestResult.BadEmailAddress.some
              )
              .some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(
              routes.CannotSendVerificationPasscodeController.cannotSendVerificationPasscode(),
              session
            )(
              mockPreviousCall
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("cannotSendVerificationPasscode.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url
              val htmlBody = doc.select(".govuk-body").html()

              val firstUrl =
                if (emailAddress.isDefined) routes.ConfirmEmailAddressController.confirmEmailAddress().url
                else routes.EnterEmailAddressController.enterEmailAddress().url

              htmlBody should include regex messageFromMessageKey(
                "cannotSendVerificationPasscode.p2",
                firstUrl,
                routes.StartController.start().url
              )

            }
          )
        }

        "there is email id in GGAccount" in {
          test(ggEmailId.some)
        }

        "there is no email in GGAccount" in {
          test(None)
        }

      }

    }
  }
}
