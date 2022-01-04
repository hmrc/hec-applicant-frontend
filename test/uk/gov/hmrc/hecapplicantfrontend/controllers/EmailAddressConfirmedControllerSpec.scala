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
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthenticatedRequest
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error, HECSession, HECTaxCheck, HECTaxCheckCode}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.{EmailParameters, EmailSendResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, SendEmailService}
import uk.gov.hmrc.hecapplicantfrontend.util.{TimeProvider, TimeUtils}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailAddressConfirmedControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider     = mock[TimeProvider]
  val mockSendEmailService = mock[SendEmailService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[SendEmailService].toInstance(mockSendEmailService)
  )

  val controller                          = instanceOf[EmailAddressConfirmedController]
  def mockTimeProviderToday(d: LocalDate) = (mockTimeProvider.currentDate _).expects().returning(d)

  val ggEmailId = EmailAddress("user@test.com")

  val userEmailAnswer = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
      passcode = Passcode("HHHHHH").some,
      passcodeVerificationResult = PasscodeVerificationResult.Match.some
    )

  val hecTaxCheckCode = HECTaxCheckCode("ABC 123 DER")

  val currentDate = LocalDate.of(2021, 7, 10)
  val expiryDate  = LocalDate.of(2021, 10, 9)

  val currentDateString = TimeUtils.govDisplayFormat(currentDate)
  val expiryDateString  = TimeUtils.govDisplayFormat(expiryDate)

  val hecTaxCheck = HECTaxCheck(hecTaxCheckCode, expiryDate)

  val emailParameters = EmailParameters("Dummy name", "10 July 2021, ABC 123 DER, 9 October 2021")

  def mockSendEmail(emailAddress: EmailAddress, emailParameters: EmailParameters)(
    result: Either[Error, EmailSendResult]
  ) =
    (mockSendEmailService
      .sendEmail(_: EmailAddress, _: EmailParameters)(_: HeaderCarrier, _: AuthenticatedRequest[_]))
      .expects(emailAddress, emailParameters, *, *)
      .returning(EitherT.fromEither[Future](result))

  "EmailAddressConfirmedControllerSpec" when {

    "handling request to email address confirmed page" must {

      def performAction(): Future[Result] = controller.emailAddressConfirmed(FakeRequest())

      "return a technical error" when {

        "user selected email is not in session" in {
          val session = Fixtures.individualHECSession(
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

      "display the page" in {
        val session: HECSession = Fixtures.companyHECSession(
          loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
          userAnswers = Fixtures.completeCompanyUserAnswers(),
          isEmailRequested = true,
          userEmailAnswers = userEmailAnswer.some
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
              .attr("action") shouldBe routes.EmailAddressConfirmedController.emailAddressConfirmedSubmit().url
          }
        )
      }

    }

    "handling submit to email address confirmed page" must {

      def performAction(): Future[Result] =
        controller.emailAddressConfirmedSubmit(FakeRequest().withFormUrlEncodedBody())

      "return a technical error" when {

        "user selected email is not in session" in {
          val session = Fixtures.individualHECSession(
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

        "Call to send email fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            completedTaxCheck = hecTaxCheck.some,
            userEmailAnswers = userEmailAnswer.some
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(currentDate)
            mockSendEmail(ggEmailId, emailParameters = emailParameters)(Left(Error("")))
          }

          assertThrows[RuntimeException](await(performAction()))
        }

        "Call to update and Next fails" in {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            completedTaxCheck = hecTaxCheck.some,
            userEmailAnswers = userEmailAnswer.some
          )

          val updatedSession =
            session.copy(userEmailAnswers = userEmailAnswer.copy(emailSendResult = EmailSendResult.EmailSent.some).some)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(currentDate)
            mockSendEmail(ggEmailId, emailParameters = emailParameters)(Right(EmailSendResult.EmailSent))
            mockJourneyServiceUpdateAndNext(
              routes.EmailAddressConfirmedController.emailAddressConfirmed(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "redirect to the next page" in {
        val session = Fixtures.companyHECSession(
          loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
          userAnswers = Fixtures.completeCompanyUserAnswers(),
          isEmailRequested = true,
          completedTaxCheck = hecTaxCheck.some,
          userEmailAnswers = userEmailAnswer.some
        )

        val updatedSession =
          session.copy(userEmailAnswers = userEmailAnswer.copy(emailSendResult = EmailSendResult.EmailSent.some).some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockTimeProviderToday(currentDate)
          mockSendEmail(ggEmailId, emailParameters = emailParameters)(Right(EmailSendResult.EmailSent))
          mockJourneyServiceUpdateAndNext(
            routes.EmailAddressConfirmedController.emailAddressConfirmed(),
            session,
            updatedSession
          )(Right(mockNextCall))
        }
        checkIsRedirect(performAction(), mockNextCall)
      }

    }
  }

}
