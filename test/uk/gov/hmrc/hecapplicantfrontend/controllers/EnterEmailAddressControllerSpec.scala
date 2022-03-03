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
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.{EmailAddressAlreadyVerified, PasscodeSent}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, Error, UserEmailAnswers, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EnterEmailAddressControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockEmailVerificationService = mock[EmailVerificationService]
  override def overrideBindings    = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[EmailVerificationService].toInstance(mockEmailVerificationService)
  )

  val controller = instanceOf[EnterEmailAddressController]

  def mockRequestPasscode(userSelectedEmail: UserSelectedEmail)(result: Either[Error, PasscodeRequestResult]) =
    (mockEmailVerificationService
      .requestPasscode(_: UserSelectedEmail)(_: HeaderCarrier, _: RequestWithSessionData[_]))
      .expects(userSelectedEmail, *, *)
      .returning(EitherT.fromEither[Future](result))

  "EnterEmailAddressControllerSpec" when {
    val otherEmailId  = EmailAddress("user1@test.com")
    val otherEmailId1 = EmailAddress("user11@test.com")
    val invalidEmail  = EmailAddress("user1@test@test.com")

    "handling request to enter email address page" must {
      def performAction(): Future[Result] = controller.enterEmailAddress(FakeRequest())

      "throw technical error " when {

        "call to update session fails" in {
          val session = Fixtures.individualHECSession(
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            hasResentEmailConfirmation = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(hasResentEmailConfirmation = false))(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "an email has not been requested" in {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some
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
            loginData = Fixtures.companyLoginData(),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = userEmailAnswers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(session.copy(hasResentEmailConfirmation = false))(Right(()))
            mockJourneyServiceGetPrevious(routes.EnterEmailAddressController.enterEmailAddress(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("enterEmailAddress.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              doc.select(".govuk-input").attr("value") shouldBe userEmailAnswers
                .map(_.userSelectedEmail.emailAddress.value)
                .getOrElse("")

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.EnterEmailAddressController.enterEmailAddressSubmit().url
            }
          )
        }

        "user hasn't  already answered the question" in {
          test(None)
        }

        "user has  already answered the question" in {

          test(Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId).some)

        }

      }
    }

    "handling submit to enter email address page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.enterEmailAddressSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      val session = Fixtures.companyHECSession(
        loginData = Fixtures.companyLoginData(),
        emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some
      )

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        def test(inputAnswer: Option[List[(String, String)]], errorMessageKey: String) = {

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EnterEmailAddressController.enterEmailAddress(), session)(
              mockPreviousCall
            )
          }

          val answers = inputAnswer.getOrElse(Nil)
          checkFormErrorIsDisplayed(
            performAction(answers: _*),
            messageFromMessageKey("enterEmailAddress.title"),
            messageFromMessageKey(errorMessageKey)
          )

        }

        "nothing is entered" in {
          test(None, "enterEmailAddress.error.required")
        }

        "invalid email address is entered" in {
          test(Some(List("enterEmailAddress" -> invalidEmail.value)), "enterEmailAddress.error.invalidFormat")
        }

        "email address is more than 256 char long" in {
          test(Some(List("enterEmailAddress" -> "u" * 257)), "enterEmailAddress.error.tooManyChar")
        }

      }

      "return a technical error" when {

        "an email has not been requested" in {
          val session = Fixtures.individualHECSession(
            loginData = Fixtures.individualLoginData(),
            emailRequestedForTaxCheck = None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }

        "Call to request passcode fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(UserSelectedEmail(EmailType.DifferentEmail, otherEmailId))(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction("enterEmailAddress" -> otherEmailId.value)))
        }

        "Call to update and Next fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some
          )

          val updatedSession =
            session.copy(userEmailAnswers =
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, PasscodeSent.some).some
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(UserSelectedEmail(EmailType.DifferentEmail, otherEmailId))(Right(PasscodeSent))
            mockJourneyServiceUpdateAndNext(
              routes.EnterEmailAddressController.enterEmailAddress(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("enterEmailAddress" -> otherEmailId.value)))
        }

      }

      "redirect to the next page" when {

        def test(
          existingUserEmailAnswers: Option[UserEmailAnswers],
          updatedUserEmailAnswers: UserEmailAnswers,
          answers: List[(String, String)]
        ) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = existingUserEmailAnswers
          )

          val updatedSession =
            session.copy(userEmailAnswers = updatedUserEmailAnswers.some)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockRequestPasscode(updatedUserEmailAnswers.userSelectedEmail)(
              Right(updatedUserEmailAnswers.passcodeRequestResult.getOrElse(PasscodeSent))
            )
            mockJourneyServiceUpdateAndNext(
              routes.EnterEmailAddressController.enterEmailAddress(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }
          checkIsRedirect(performAction(answers: _*), mockNextCall)
        }
        "user hasn't previously answered the questions" in {
          test(
            existingUserEmailAnswers = None,
            updatedUserEmailAnswers =
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, PasscodeSent.some),
            answers = List("enterEmailAddress" -> otherEmailId.value)
          )
        }

        "user has previously answered the questions" in {

          test(
            existingUserEmailAnswers =
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, PasscodeSent.some).some,
            updatedUserEmailAnswers = Fixtures.userEmailAnswers(
              EmailType.DifferentEmail,
              otherEmailId1,
              EmailAddressAlreadyVerified.some
            ),
            answers = List("enterEmailAddress" -> otherEmailId1.value)
          )

        }

      }
    }

  }

}
