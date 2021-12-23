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
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthenticatedRequest
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.{EmailAddressAlreadyVerified, PasscodeSent}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, Error, UserEmailAnswers}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ConfirmEmailAddressControllerSpec
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

  val controller = instanceOf[ConfirmEmailAddressController]

  def mockRequestPasscode(email: EmailAddress)(result: Either[Error, PasscodeRequestResult]) =
    (mockEmailVerificationService
      .requestPasscode(_: EmailAddress)(_: HeaderCarrier, _: AuthenticatedRequest[_]))
      .expects(email, *, *)
      .returning(EitherT.fromEither[Future](result))

  "ConfirmEmailAddressControllerSpec" when {
    val ggEmailId    = EmailAddress("user@test.com")
    val otherEmailId = EmailAddress("user1@test.com")
    val invalidEmail = EmailAddress("user1@test@test.com")

    "handling request to confirm email address page" must {
      def performAction(): Future[Result] = controller.confirmEmailAddress(FakeRequest())

      "return a technical error" when {

        "GG account email id is not in session" in {
          val session = Fixtures.individualHECSession(
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "display the page" when {

        def test(userEmailAnswers: Option[UserEmailAnswers], selected: Option[String]) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = EmailAddress("user@test.com").some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = userEmailAnswers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.ConfirmEmailAddressController.confirmEmailAddress(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmEmailAddress.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selected match {
                case Some(value) => selectedOptions.attr("value") shouldBe value
                case None        => selectedOptions.isEmpty       shouldBe true
              }
              doc.select("#differentEmail").attr("value") shouldBe userEmailAnswers
                .flatMap(_.emailAddress.map(_.value))
                .getOrElse("")

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.ConfirmEmailAddressController.confirmEmailAddressSubmit().url
            }
          )
        }

        "User's GG account has a valid email id and user hasn't previously answered the questions " in {
          test(userEmailAnswers = None, selected = None)
        }

        "User's GG account has a valid email id and user has previously selected ggEmail id  " in {
          test(UserEmailAnswers(EmailType.GGEmail, None, None, None).some, Some("0"))
        }

        "User's GG account has a valid email id and user has previously selected different email id option" in {
          test(UserEmailAnswers(EmailType.DifferentEmail, otherEmailId.some, None, None).some, Some("1"))

        }
      }
    }

    "handling submit to confirm email address page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.confirmEmailAddressSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      val session = Fixtures.companyHECSession(
        loginData = Fixtures.companyLoginData(emailAddress = EmailAddress("user@test.com").some),
        userAnswers = Fixtures.completeCompanyUserAnswers(),
        isEmailRequested = true
      )

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {
        def test(
          inputAnswer: Option[List[(String, String)]],
          errorMessageKey: String
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.ConfirmEmailAddressController.confirmEmailAddress(), session)(
              mockPreviousCall
            )
          }

          val answers = inputAnswer.getOrElse(Nil)

          checkFormErrorIsDisplayed(
            performAction(answers: _*),
            messageFromMessageKey("confirmEmailAddress.title"),
            messageFromMessageKey(errorMessageKey)
          )

        }

        "No radio button is selected" in {
          test(None, "confirmEmailAddress.error.required")
        }

        "an index is submitted which is too large" in {
          test(Some(List("confirmEmailAddress" -> "5")), "confirmEmailAddress.error.invalid")
          test(Some(List("confirmEmailAddress" -> Int.MaxValue.toString)), "confirmEmailAddress.error.invalid")
        }

        "a value is submitted which is not a number" in {
          test(Some(List("confirmEmailAddress" -> "xyz")), "confirmEmailAddress.error.invalid")
        }

        "a correct index (1) with value(A different email address) is selected , but the email address is not entered" in {
          test(Some(List("confirmEmailAddress" -> "1")), "differentEmail.error.required")
        }

        "a correct index (1) with value(A different email address) is selected, but the invalid email address is entered" in {

          test(
            Some(List("confirmEmailAddress" -> "1", "differentEmail" -> invalidEmail.value)),
            "differentEmail.error.invalidFormat"
          )
        }

        "a correct index (1) with value(A different email address) is selected, but the email address entered is more than 256 chars" in {

          test(
            Some(List("confirmEmailAddress" -> "1", "differentEmail" -> "u" * 257)),
            "differentEmail.error.tooManyChar"
          )

        }

      }

      "return a technical error" when {

        "no ggEmail is found in the session" in {
          val session = Fixtures.individualHECSession(
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "Call to request passcode fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(ggEmailId)(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction("confirmEmailAddress" -> "0")))
        }

        "Call to update and Next fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true
          )

          val updatedSession =
            session.copy(userEmailAnswers = UserEmailAnswers(EmailType.GGEmail, None, PasscodeSent.some, None).some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(ggEmailId)(Right(PasscodeSent))
            mockJourneyServiceUpdateAndNext(
              routes.ConfirmEmailAddressController.confirmEmailAddress(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("confirmEmailAddress" -> "0")))
        }

      }

      "redirect to the next page" when {

        def test(
          existingUserEmailAnswers: Option[UserEmailAnswers],
          updatedUserEmailAnswers: UserEmailAnswers,
          answers: List[(String, String)]
        ) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = existingUserEmailAnswers
          )

          val updatedSession =
            session.copy(userEmailAnswers = updatedUserEmailAnswers.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(updatedUserEmailAnswers.emailAddress.getOrElse(ggEmailId))(
              Right(updatedUserEmailAnswers.passcodeRequestResult.getOrElse(PasscodeSent))
            )
            mockJourneyServiceUpdateAndNext(
              routes.ConfirmEmailAddressController.confirmEmailAddress(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }
          checkIsRedirect(performAction(answers: _*), mockNextCall)
        }

        "user hasn't previously answered the questions" in {

          test(
            existingUserEmailAnswers = None,
            updatedUserEmailAnswers = UserEmailAnswers(EmailType.GGEmail, None, PasscodeSent.some, None),
            answers = List("confirmEmailAddress" -> "0")
          )

        }

        "user has previously answered the questions" in {

          test(
            existingUserEmailAnswers = UserEmailAnswers(EmailType.GGEmail, None, PasscodeSent.some, None).some,
            updatedUserEmailAnswers =
              UserEmailAnswers(EmailType.DifferentEmail, otherEmailId.some, EmailAddressAlreadyVerified.some, None),
            answers = List("confirmEmailAddress" -> "1", "differentEmail" -> otherEmailId.value)
          )

        }

      }
    }

  }
}
