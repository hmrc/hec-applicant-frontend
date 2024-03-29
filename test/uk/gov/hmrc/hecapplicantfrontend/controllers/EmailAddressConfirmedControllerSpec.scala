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

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import play.api.i18n.Lang
import play.api.inject.bind
import play.api.mvc.{Cookie, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailRequestedForTaxCheck, EmailType, Error, HECSession, HECTaxCheckCode, TaxCheckListItem, UserEmailAnswers, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.{EmailParameters, EmailSendResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, SendEmailService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailAddressConfirmedControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockSendEmailService = mock[SendEmailService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[SendEmailService].toInstance(mockSendEmailService)
  )

  val controller = instanceOf[EmailAddressConfirmedController]

  val ggUserSelectedEmail = UserSelectedEmail(EmailType.GGEmail, EmailAddress("user@test.com"))

  val passcodeSentAndMatchedUserEmailAnswer = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
      passcode = Passcode("HHHHHH").some,
      passcodeVerificationResult = PasscodeVerificationResult.Match.some
    )

  val emailAlreadyVerifiedUserEmailAnswers = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.EmailAddressAlreadyVerified.some,
      passcode = None,
      passcodeVerificationResult = None
    )

  val hecTaxCheckCode = HECTaxCheckCode("ABC 123 DER")

  val expiryDate = LocalDate.of(2021, 10, 9)
  val createDate = ZonedDateTime.of(2021, 7, 9, 0, 0, 0, 0, ZoneId.of("Europe/London"))

  val createDateString = TimeUtils.govDisplayFormat(createDate.toLocalDate)
  val expiryDateString = TimeUtils.govDisplayFormat(expiryDate)

  val emailRequestedForTaxCheck =
    EmailRequestedForTaxCheck(
      "",
      TaxCheckListItem(LicenceType.DriverOfTaxisAndPrivateHires, hecTaxCheckCode, expiryDate, createDate)
    )

  val emailParametersEN =
    EmailParameters(
      "9 July 2021",
      messagesApi("licenceType.driverOfTaxis")(Lang("en")),
      "ABC 123 DER",
      "9 October 2021"
    )
  val emailParametersCY = EmailParameters(
    "9 Gorffennaf 2021",
    messagesApi("licenceType.driverOfTaxis")(Lang("cy")),
    "ABC 123 DER",
    "9 Hydref 2021"
  )

  def mockSendEmail(userSelectedEmail: UserSelectedEmail, emailParameters: EmailParameters)(
    result: Either[Error, EmailSendResult]
  ) =
    (mockSendEmailService
      .sendEmail(_: UserSelectedEmail, _: EmailParameters)(_: HeaderCarrier, _: RequestWithSessionData[_]))
      .expects(userSelectedEmail, emailParameters, *, *)
      .returning(EitherT.fromEither[Future](result))

  "EmailAddressConfirmedControllerSpec" when {

    "handling request to email address confirmed page" must {

      def performAction(): Future[Result] = controller.emailAddressConfirmed(FakeRequest())

      "return a technical error" when {

        def isError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "user selected email is not in session" in {
          val session = Fixtures.individualHECSession(
            emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
            userEmailAnswers = None
          )
          isError(session)

        }

        "passcode verification code is other than match and passcodeRequestResult = PasscodeSent" in {
          List(
            PasscodeVerificationResult.NoMatch.some,
            PasscodeVerificationResult.Expired.some,
            PasscodeVerificationResult.TooManyAttempts.some
          ).foreach { passcodeVerificationResult =>
            withClue(s" For passcode verification result $passcodeVerificationResult") {
              val session = Fixtures.individualHECSession(
                emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                    passcode = Passcode("HHHHHH").some,
                    passcodeVerificationResult = passcodeVerificationResult,
                    emailSendResult = None
                  )
                  .some
              )
              isError(session)
            }
          }

        }

        "passcodeVerificationCode ==  Match and passcodeRequestResult != PasscodeSent/'Already Verified'" in {
          List(
            PasscodeRequestResult.BadEmailAddress.some,
            PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some
          ).foreach { passcodeRequestResult =>
            withClue(s" For passcode verification result $passcodeRequestResult") {
              val session = Fixtures.individualHECSession(
                emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = passcodeRequestResult,
                    passcode = Passcode("HHHHHH").some,
                    passcodeVerificationResult = PasscodeVerificationResult.Match.some,
                    emailSendResult = None
                  )
                  .some
              )
              isError(session)
            }
          }

        }

        "passcodeVerificationCode ==  None and passcodeRequestResult != 'Already Verified'" in {
          List(
            PasscodeRequestResult.PasscodeSent.some,
            PasscodeRequestResult.BadEmailAddress.some,
            PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some
          ).foreach { passcodeRequestResult =>
            withClue(s" For passcode verification result $passcodeRequestResult") {
              val session = Fixtures.individualHECSession(
                emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = passcodeRequestResult,
                    passcode = Passcode("HHHHHH").some,
                    passcodeVerificationResult = None,
                    emailSendResult = None
                  )
                  .some
              )
              isError(session)
            }
          }
        }

      }

      "display the page" when {

        def test(userEmailAnswers: UserEmailAnswers) = {
          val session: HECSession = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggUserSelectedEmail.emailAddress.some),
            emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
            userEmailAnswers = userEmailAnswers.some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EmailAddressConfirmedController.emailAddressConfirmed, session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("emailAddressConfirmed.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              doc.select(".govuk-body").text() should include regex "user@test.com"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.EmailAddressConfirmedController.emailAddressConfirmedSubmit.url
            }
          )
        }

        "the passcode has been sent and the passcode verification result is a match" in {
          test(passcodeSentAndMatchedUserEmailAnswer)
        }

        "the email addres had already been verified" in {
          test(emailAlreadyVerifiedUserEmailAnswers)
        }
      }

    }

    "handling submit to email address confirmed page" must {

      def performAction(): Future[Result] =
        controller.emailAddressConfirmedSubmit(FakeRequest().withMethod(POST))

      def performActionCY(): Future[Result] =
        controller.emailAddressConfirmedSubmit(FakeRequest().withMethod(POST).withCookies(Cookie("PLAY_LANG", "cy")))

      "return a technical error" when {

        def testInconsistentSessionStateError(session: HECSession) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }

        "user selected email is not in session" in {
          val session = Fixtures.individualHECSession(
            emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
            userEmailAnswers = None
          )
          testInconsistentSessionStateError(session)
        }

        "user has not chosen to get an email sent to them" in {
          val session = Fixtures.individualHECSession(
            emailRequestedForTaxCheck = None,
            userEmailAnswers = Some(Fixtures.userEmailAnswers())
          )
          testInconsistentSessionStateError(session)
        }

        "passcode verification code is other than match and passcodeRequestResult = PasscodeSent" in {
          List(
            PasscodeVerificationResult.NoMatch.some,
            PasscodeVerificationResult.Expired.some,
            PasscodeVerificationResult.TooManyAttempts.some
          ).foreach { passcodeVerificationResult =>
            withClue(s" For passcode verification result $passcodeVerificationResult") {
              val session = Fixtures.individualHECSession(
                emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                    passcode = Passcode("HHHHHH").some,
                    passcodeVerificationResult = passcodeVerificationResult,
                    emailSendResult = None
                  )
                  .some
              )
              testInconsistentSessionStateError(session)
            }
          }

        }

        "passcodeVerificationCode ==  Match and passcodeRequestResult != PasscodeSent/'Already Verified'" in {
          List(
            PasscodeRequestResult.BadEmailAddress.some,
            PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some
          ).foreach { passcodeRequestResult =>
            withClue(s" For passcode verification result $passcodeRequestResult") {
              val session = Fixtures.individualHECSession(
                emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = passcodeRequestResult,
                    passcode = Passcode("HHHHHH").some,
                    passcodeVerificationResult = PasscodeVerificationResult.Match.some,
                    emailSendResult = None
                  )
                  .some
              )
              testInconsistentSessionStateError(session)
            }
          }

        }

        "passcodeVerificationCode ==  None and passcodeRequestResult != 'Already Verified'" in {
          List(
            PasscodeRequestResult.PasscodeSent.some,
            PasscodeRequestResult.BadEmailAddress.some,
            PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some
          ).foreach { passcodeRequestResult =>
            withClue(s" For passcode verification result $passcodeRequestResult") {
              val session = Fixtures.individualHECSession(
                emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
                userEmailAnswers = Fixtures
                  .userEmailAnswers(
                    passcodeRequestResult = passcodeRequestResult,
                    passcode = Passcode("HHHHHH").some,
                    passcodeVerificationResult = None,
                    emailSendResult = None
                  )
                  .some
              )
              testInconsistentSessionStateError(session)
            }
          }
        }

        "Call to send email fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggUserSelectedEmail.emailAddress.some),
            emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
            userEmailAnswers = passcodeSentAndMatchedUserEmailAnswer.some,
            userAnswers = Fixtures.completeCompanyUserAnswers(licenceType = LicenceType.BookingOffice)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSendEmail(
              ggUserSelectedEmail,
              emailParameters = emailParametersEN.copy(licenceType = messageFromMessageKey("licenceType.driverOfTaxis"))
            )(Left(Error("")))
          }

          assertThrows[RuntimeException](await(performAction()))
        }

        "Call to update and Next fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggUserSelectedEmail.emailAddress.some),
            emailRequestedForTaxCheck = emailRequestedForTaxCheck.some,
            userEmailAnswers = passcodeSentAndMatchedUserEmailAnswer.some
          )

          val updatedSession =
            session.copy(userEmailAnswers =
              passcodeSentAndMatchedUserEmailAnswer
                .copy(
                  emailSendResult = EmailSendResult.EmailSent.some,
                  passcodeRequestResult = None,
                  passcode = None,
                  passcodeVerificationResult = None
                )
                .some
            )
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSendEmail(ggUserSelectedEmail, emailParameters = emailParametersEN)(Right(EmailSendResult.EmailSent))
            mockJourneyServiceUpdateAndNext(
              routes.EmailAddressConfirmedController.emailAddressConfirmed,
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

        def testRedirect(
          emailParameters: EmailParameters,
          isEnglish: Boolean,
          emailSendSuccessful: Boolean
        ) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggUserSelectedEmail.emailAddress.some),
            emailRequestedForTaxCheck = emailRequestedForTaxCheck
              .copy(taxCheck = emailRequestedForTaxCheck.taxCheck.copy(taxCheckCode = HECTaxCheckCode("ABC123DER")))
              .some,
            userEmailAnswers = passcodeSentAndMatchedUserEmailAnswer.some
          )

          val emailSendResult = if (emailSendSuccessful) EmailSendResult.EmailSent else EmailSendResult.EmailSentFailure

          val updatedSession =
            if (emailSendSuccessful)
              session.copy(userEmailAnswers =
                passcodeSentAndMatchedUserEmailAnswer
                  .copy(
                    emailSendResult = emailSendResult.some,
                    passcodeRequestResult = None,
                    passcode = None,
                    passcodeVerificationResult = None
                  )
                  .some
              )
            else
              session.copy(userEmailAnswers =
                passcodeSentAndMatchedUserEmailAnswer
                  .copy(
                    emailSendResult = emailSendResult.some
                  )
                  .some
              )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSendEmail(ggUserSelectedEmail, emailParameters = emailParameters)(Right(emailSendResult))
            mockJourneyServiceUpdateAndNext(
              routes.EmailAddressConfirmedController.emailAddressConfirmed,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          if (isEnglish) checkIsRedirect(performAction(), mockNextCall)
          else checkIsRedirect(performActionCY(), mockNextCall)
        }

        "the email is successfully sent and" when {

          "the user has selected  English language" in {
            testRedirect(emailParametersEN, isEnglish = true, emailSendSuccessful = true)
          }

          "the user has selected  welsh language" in {
            testRedirect(emailParametersCY, isEnglish = false, emailSendSuccessful = true)
          }

        }

        "the email is not successfully sent and" when {

          "the user has selected  English language" in {
            testRedirect(emailParametersEN, isEnglish = true, emailSendSuccessful = false)
          }

          "the user has selected  welsh language" in {
            testRedirect(emailParametersCY, isEnglish = false, emailSendSuccessful = false)
          }

        }

      }

    }
  }

}
