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

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.PasscodeSent
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeVerificationResult.Match
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error, UserEmailAnswers}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class VerifyEmailPasscodeControllerSpec
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

  val controller = instanceOf[VerifyEmailPasscodeController]

  def mockVerifyPasscode(passcode: Passcode, emailAddress: EmailAddress)(
    result: Either[Error, PasscodeVerificationResult]
  ) =
    (mockEmailVerificationService
      .verifyPasscode(_: Passcode, _: EmailAddress)(_: HeaderCarrier))
      .expects(passcode, emailAddress, *)
      .returning(EitherT.fromEither[Future](result))

  "VerifyEmailPasscodeControllerSpec" when {
    val ggEmailId     = EmailAddress("user@test.com")
    val validPasscode = Passcode("HHHHHH")

    "handling request to verify email passcode page" must {

      def performAction(): Future[Result] = controller.verifyEmailPasscode(FakeRequest())

      "return a technical error" when {

        "there is no user selected email in session " in {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "display the page" when {

        def test(userEmailAnswers: Option[UserEmailAnswers]) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = userEmailAnswers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.VerifyEmailPasscodeController.verifyEmailPasscode(), session)(
              mockPreviousCall
            )
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("verifyPasscode.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val textBody = doc.select(".govuk-body").text()
              val htmlBody = doc.select(".govuk-body").html()

              textBody should include regex userEmailAnswers
                .map(_.userSelectedEmail.emailAddress.value)
                .getOrElse("")
              htmlBody should include regex messageFromMessageKey(
                "verifyPasscode.p5",
                routes.ResendEmailConfirmationController.resendEmail().url,
                routes.TaxCheckCompleteController.taxCheckComplete().url
              )

              doc.select("#passcode").attr("value") shouldBe userEmailAnswers
                .flatMap(_.passcode.map(_.value))
                .getOrElse("")

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.VerifyEmailPasscodeController.verifyEmailPasscodeSubmit().url
            }
          )

        }

        "User hasn't  previously entered the passcode" in {

          test(userEmailAnswers = Fixtures.userEmailAnswers().some)

        }

        "User has previously entered the passcode" in {

          test(userEmailAnswers =
            Fixtures
              .userEmailAnswers(
                passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                passcode = validPasscode.some
              )
              .some
          )

        }
      }

    }

    "handling submit to verify email passcode page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.verifyEmailPasscodeSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        "nothing is entered" in {

          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers =
              Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some).some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.VerifyEmailPasscodeController.verifyEmailPasscode(), session)(
              mockPreviousCall
            )
          }
          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("verifyPasscode.title"),
            messageFromMessageKey("passcode.error.required")
          )

        }

      }

      "return a technical error" when {

        "there is no user selected email in session " in {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "call to verify passcode fails" in {

          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = Fixtures
              .userEmailAnswers(
                passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                passcode = validPasscode.some
              )
              .some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockVerifyPasscode(validPasscode, ggEmailId)(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction("passcode" -> validPasscode.value)))

        }

        "call to update and next fails" in {

          val userEmailAnswers = Fixtures
            .userEmailAnswers(
              passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
              passcode = validPasscode.some
            )

          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = userEmailAnswers.some
          )

          val updatedSession =
            session.copy(userEmailAnswers = userEmailAnswers.copy(passcodeVerificationResult = Match.some).some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockVerifyPasscode(validPasscode, ggEmailId)(Right(Match))
            mockJourneyServiceUpdateAndNext(
              routes.VerifyEmailPasscodeController.verifyEmailPasscode(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("passcode" -> validPasscode.value)))

        }
      }

      "redirect to next page" when {

        val passcodeVerificationList = List(
          PasscodeVerificationResult.Match,
          PasscodeVerificationResult.NoMatch,
          PasscodeVerificationResult.Expired,
          PasscodeVerificationResult.TooManyAttempts
        )
        def test(
          existingUserEmailAnswers: UserEmailAnswers,
          updatedUserEmailAnswers: UserEmailAnswers,
          passcodeVerificationResult: PasscodeVerificationResult,
          answers: List[(String, String)]
        ) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = existingUserEmailAnswers.some
          )

          val updatedSession =
            session.copy(userEmailAnswers = updatedUserEmailAnswers.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockVerifyPasscode(validPasscode, ggEmailId)(Right(passcodeVerificationResult))
            mockJourneyServiceUpdateAndNext(
              routes.VerifyEmailPasscodeController.verifyEmailPasscode(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction(answers: _*), mockNextCall)
        }

        "user hasn't previously answered the question" in {
          passcodeVerificationList.foreach { passcodeVerificationResult =>
            withClue(s" For PasscodeVerificationResult :: $passcodeVerificationResult") {
              val existingUserEmailAnswers = Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeSent.some)
              val updatedUserEmailAnswers  =
                Fixtures.userEmailAnswers(
                  passcodeRequestResult = PasscodeSent.some,
                  passcode = validPasscode.some,
                  passcodeVerificationResult = passcodeVerificationResult.some
                )

              test(
                existingUserEmailAnswers,
                updatedUserEmailAnswers,
                passcodeVerificationResult,
                List("passcode" -> validPasscode.value)
              )
            }

          }
        }

        "user has previously answered the question" in {

          passcodeVerificationList.foreach { passcodeVerificationResult =>
            withClue(s" For PasscodeVerificationResult :: $passcodeVerificationResult") {
              val existingUserEmailAnswers = Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeSent.some)
              val updatedUserEmailAnswers  =
                Fixtures.userEmailAnswers(
                  passcodeRequestResult = PasscodeSent.some,
                  passcode = validPasscode.some,
                  passcodeVerificationResult = passcodeVerificationResult.some
                )

              test(
                existingUserEmailAnswers,
                updatedUserEmailAnswers,
                passcodeVerificationResult,
                List("passcode" -> validPasscode.value)
              )
            }

          }
        }

      }

    }

  }

}
