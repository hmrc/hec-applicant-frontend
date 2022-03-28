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

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, Error, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.PasscodeSent
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ResendEmailConfirmationControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {
  val mockEmailVerificationService = mock[EmailVerificationService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[EmailVerificationService].toInstance(mockEmailVerificationService)
  )
  val ggEmailId                 = EmailAddress("user@test.com")
  val userSelectedGGEmail       = UserSelectedEmail(EmailType.GGEmail, ggEmailId)
  val controller                = instanceOf[ResendEmailConfirmationController]

  def mockRequestPasscode(userSelectedEmail: UserSelectedEmail)(result: Either[Error, PasscodeRequestResult]) =
    (mockEmailVerificationService
      .requestPasscode(_: UserSelectedEmail)(_: HeaderCarrier, _: RequestWithSessionData[_]))
      .expects(userSelectedEmail, *, *)
      .returning(EitherT.fromEither[Future](result))

  "ResendEmailConfirmationControllerSpec" when {

    "handling request on the resend email confirmation page " must {
      def performAction(): Future[Result] = controller.resendEmail(FakeRequest())

      "return a technical error" when {

        "user selected email is not in session" in {
          val session = Fixtures.individualHECSession(
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }

      }

      "display the page" in {

        val session = Fixtures.individualHECSession(
          emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
          userEmailAnswers = Fixtures
            .userEmailAnswers(
              passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some
            )
            .some,
          hasResentEmailConfirmation = true
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.ResendEmailConfirmationController.resendEmail, session)(
            mockPreviousCall
          )
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("resendEmailConfirmation.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            doc.select(".govuk-body").text() should include regex "user@test.com"

            val form = doc.select("form")
            form
              .attr("action") shouldBe routes.ResendEmailConfirmationController.resendEmailSubmit.url
          }
        )
      }

    }

    "handling submit to resend email confirmation page " must {
      def performAction(): Future[Result] =
        controller.resendEmailSubmit(FakeRequest().withFormUrlEncodedBody())

      "return a technical error" when {

        "user selected email is not in session" in {
          val session = Fixtures.individualHECSession(
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "Call to request passcode fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = Fixtures
              .userEmailAnswers(
                passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some
              )
              .some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(userSelectedGGEmail)(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "Call to update and Next fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = Fixtures.userEmailAnswers().some
          )

          val updatedSession =
            session.copy(
              userEmailAnswers = Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeSent.some).some,
              hasResentEmailConfirmation = true
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(userSelectedGGEmail)(Right(PasscodeSent))
            mockJourneyServiceUpdateAndNext(
              routes.ResendEmailConfirmationController.resendEmail,
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "redirect to next page" in {
        val session = Fixtures.companyHECSession(
          loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
          emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
          userEmailAnswers = Fixtures.userEmailAnswers().some
        )

        val updatedSession =
          session.copy(
            userEmailAnswers = Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeSent.some).some,
            hasResentEmailConfirmation = true
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockRequestPasscode(userSelectedGGEmail)(Right(PasscodeSent))
          mockJourneyServiceUpdateAndNext(
            routes.ResendEmailConfirmationController.resendEmail,
            session,
            updatedSession
          )(Right(mockNextCall))
        }
        checkIsRedirect(performAction(), mockNextCall)
      }

    }

  }

}
