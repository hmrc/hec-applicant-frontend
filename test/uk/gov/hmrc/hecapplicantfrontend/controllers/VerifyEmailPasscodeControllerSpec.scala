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
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.PasscodeSent
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeVerificationResult.Match
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, Error, HECSession, UserEmailAnswers, UserSelectedEmail}
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

  def mockVerifyPasscode(passcode: Passcode, userSelectedEmail: UserSelectedEmail)(
    result: Either[Error, PasscodeVerificationResult]
  ) =
    (mockEmailVerificationService
      .verifyPasscode(_: Passcode, _: UserSelectedEmail)(_: HeaderCarrier, _: RequestWithSessionData[_]))
      .expects(passcode, userSelectedEmail, *, *)
      .returning(EitherT.fromEither[Future](result))

  "VerifyEmailPasscodeControllerSpec" when {
    val ggEmailId           = EmailAddress("user@test.com")
    val userSelectedGGEmail = UserSelectedEmail(EmailType.GGEmail, ggEmailId)

    val validPasscode = Passcode("HHHHHH")

    "handling request to verify email passcode page" must {

      def performAction(): Future[Result] = controller.verifyEmailPasscode(FakeRequest())

      "return a technical error" when {

        def testTechnicalError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "there is no user selected email in session " in {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = None
          )
          testTechnicalError(session)
        }

        "session failed to get updated" in {

          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers =
              Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some).some
          )
          testTechnicalError(session)
        }
      }

      "display the page" when {

        def test(
          userEmailAnswers: Option[UserEmailAnswers],
          emailAddress: Option[EmailAddress],
          hasResentEmailConfirmation: Boolean
        ) = {
          val session                 = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = emailAddress),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = userEmailAnswers,
            hasResentEmailConfirmation = hasResentEmailConfirmation
          )
          val updatedUserEmailAnswers = userEmailAnswers.map(_.copy(passcodeVerificationResult = None))
          val updatedSession          =
            session.copy(hasResentEmailConfirmation = false, userEmailAnswers = updatedUserEmailAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
            mockJourneyServiceGetPrevious(routes.VerifyEmailPasscodeController.verifyEmailPasscode(), updatedSession)(
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

              if (emailAddress.isDefined) {
                htmlBody should include regex messageFromMessageKey(
                  "verifyPasscode.p5",
                  routes.ResendEmailConfirmationController.resendEmail().url,
                  routes.ConfirmEmailAddressController.confirmEmailAddress().url
                )
              } else {
                htmlBody should include regex messageFromMessageKey(
                  "verifyPasscode.p5",
                  routes.ResendEmailConfirmationController.resendEmail().url,
                  routes.EnterEmailAddressController.enterEmailAddress().url
                )
              }

              doc.select("#passcode").attr("value") shouldBe userEmailAnswers
                .flatMap(_.passcode.map(_.value))
                .getOrElse("")

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.VerifyEmailPasscodeController.verifyEmailPasscodeSubmit().url
            }
          )

        }

        "User hasn't  previously entered the passcode and session has ggEmail Id" in {

          test(
            userEmailAnswers = Fixtures
              .userEmailAnswers(passcodeVerificationResult = PasscodeVerificationResult.TooManyAttempts.some)
              .some,
            ggEmailId.some,
            true
          )

        }

        "User hasn't  previously entered the passcode and session has no ggEmail Id" in {

          test(userEmailAnswers = Fixtures.userEmailAnswers().some, None, false)

        }

        "User has previously entered the passcode and session has ggEmail Id" in {

          test(
            userEmailAnswers = Fixtures
              .userEmailAnswers(
                passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                passcode = validPasscode.some
              )
              .some,
            ggEmailId.some,
            true
          )

        }
      }

    }

    "handling submit to verify email passcode page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.verifyEmailPasscodeSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        def testFormError(formData: Option[(String, String)])(expectedErrorMessageKey: String) = {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
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
            performAction(formData.toList: _*),
            messageFromMessageKey("verifyPasscode.title"),
            messageFromMessageKey(expectedErrorMessageKey),
            additionalChecks = { doc =>
              doc.select("#passcode").attr("value") shouldBe formData.map(_._2).getOrElse("")
            }
          )
        }

        "nothing is entered" in {
          testFormError(None)("passcode.error.required")
        }

        "the submitted code is not exactly 6 characters in length" in {
          testFormError(Some("passcode" -> "HHHHH"))("passcode.error.format")
        }

        "the submitted code is 6 characters long but contains vowels" in {
          List("A", "e", "i", "O", "u").foreach { vowel =>
            withClue(s"For vowel '$vowel': ") {
              testFormError(Some("passcode" -> s"HHHHH$vowel"))("passcode.error.format")
            }
          }
        }

        "No match passcode is entered" in {
          val passcode         = Passcode("FFFFFF")
          val userEmailAnswers = Fixtures
            .userEmailAnswers(
              passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some
            )

          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = userEmailAnswers.some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockVerifyPasscode(passcode, userSelectedGGEmail)(
              Right(PasscodeVerificationResult.NoMatch)
            )
            mockJourneyServiceGetPrevious(routes.VerifyEmailPasscodeController.verifyEmailPasscode(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("passcode" -> "FF FFF F"),
            messageFromMessageKey("verifyPasscode.title"),
            messageFromMessageKey("passcode.error.noMatch"),
            additionalChecks = { doc =>
              doc.select("#passcode").attr("value") shouldBe "FF FFF F"
            }
          )
        }

      }

      "return a technical error" when {

        "there is no user selected email in session " in {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
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
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
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
            mockVerifyPasscode(validPasscode, userSelectedGGEmail)(Left(Error("")))
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
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = userEmailAnswers.some
          )

          val updatedSession =
            session.copy(userEmailAnswers = userEmailAnswers.copy(passcodeVerificationResult = Match.some).some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockVerifyPasscode(validPasscode, userSelectedGGEmail)(Right(Match))
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
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = existingUserEmailAnswers.some
          )

          val updatedSession =
            session.copy(userEmailAnswers = updatedUserEmailAnswers.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockVerifyPasscode(validPasscode, userSelectedGGEmail)(
              Right(passcodeVerificationResult)
            )
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
                List("passcode" -> " hh h H  h h ")
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
