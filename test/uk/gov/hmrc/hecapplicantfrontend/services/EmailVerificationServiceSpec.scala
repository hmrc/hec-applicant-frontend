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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.http.Status.{BAD_GATEWAY, FORBIDDEN}
import play.api.i18n.Lang
import play.api.libs.json.Json
import play.api.mvc.{AnyContentAsEmpty, Cookie, MessagesRequest}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.connectors.EmailVerificationConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.ControllerSpec
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.{SubmitEmailAddressVerificationPasscode, SubmitEmailAddressVerificationRequest}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendResult.EmailSent
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult._
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeVerificationResult._
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailRequestedForTaxCheck, EmailType, Error, HECSession, HECTaxCheckCode, Language, TaxCheckListItem, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequest, PasscodeRequestResult, PasscodeVerificationRequest, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.services.EmailVerificationService.ErrorResponse
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class EmailVerificationServiceSpec
    extends AnyWordSpec
    with Matchers
    with MockFactory
    with ControllerSpec
    with AuditServiceSupport {

  val mockEmailVerificationConnector = mock[EmailVerificationConnector]

  def mockRequestPasscode(passcodeRequest: PasscodeRequest)(result: Either[Error, HttpResponse]) =
    (mockEmailVerificationConnector
      .requestPasscode(_: PasscodeRequest)(_: HeaderCarrier))
      .expects(passcodeRequest, *)
      .returning(EitherT.fromEither(result))

  def mockVerifyPasscode(passcode: Passcode, emailAddress: EmailAddress)(result: Either[Error, HttpResponse]) =
    (mockEmailVerificationConnector
      .verifyPasscode(_: PasscodeVerificationRequest)(_: HeaderCarrier))
      .expects(PasscodeVerificationRequest(passcode, emailAddress), *)
      .returning(EitherT.fromEither(result))

  val service      = new EmailVerificationServiceImpl(mockEmailVerificationConnector, mockAuditService)
  val emptyHeaders = Map.empty[String, Seq[String]]

  val ggEmailId = EmailAddress("user@test.com")

  val userEmailAnswer = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
      passcode = Passcode("HHHHHH").some,
      passcodeVerificationResult = PasscodeVerificationResult.Match.some,
      emailSendResult = EmailSent.some
    )

  val taxCheckCode = HECTaxCheckCode("code")

  val session: HECSession        = Fixtures.companyHECSession(
    loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
    emailRequestedForTaxCheck = Some(
      EmailRequestedForTaxCheck(
        "",
        TaxCheckListItem(LicenceType.ScrapMetalDealerSite, taxCheckCode, LocalDate.now, ZonedDateTime.now())
      )
    ),
    userEmailAnswers = userEmailAnswer.some
  )
  implicit val hc: HeaderCarrier = HeaderCarrier()

  override def additionalConfig     = super.additionalConfig.withFallback(
    Configuration(
      ConfigFactory.parseString(
        s"""
           | play.i18n.langs = ["en", "cy", "fr"]
           |""".stripMargin
      )
    )
  )
  implicit val authenticatedRequest = AuthenticatedRequest(new MessagesRequest(FakeRequest(), messagesApi))
  implicit val requestWithSession   = RequestWithSessionData(authenticatedRequest, session)

  val serviceNameMessageKey = "emailVerification.passcodeEmail.serviceName"
  val serviceNameEnglish    = messages.messagesApi(serviceNameMessageKey)(Lang("en"))
  val serviceNameWelsh      = messages.messagesApi(serviceNameMessageKey)(Lang("cy"))

  "EmailVerificationServiceSpec" when {
    val userSelectedEmail = UserSelectedEmail(EmailType.GGEmail, EmailAddress("user@test.com"))

    "handling request to requestPasscode" must {

      val passcodeRequest    = PasscodeRequest(userSelectedEmail.emailAddress, serviceNameEnglish, Language.English)
      val expectedAuditEvent = SubmitEmailAddressVerificationRequest(
        requestWithSession.sessionData.loginData.ggCredId,
        taxCheckCode,
        userSelectedEmail.emailAddress,
        userSelectedEmail.emailType,
        None
      )

      "return a technical  error " when {

        "Language in the session is not either en or cy" in {
          val authenticatedRequest: AuthenticatedRequest[AnyContentAsEmpty.type] = AuthenticatedRequest(
            new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "fr")), messagesApi)
          )

          implicit val requestWithSession: RequestWithSessionData[AnyContentAsEmpty.type] =
            RequestWithSessionData(authenticatedRequest, session)

          mockSendAuditEvent(expectedAuditEvent)

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http call fails" in {
          inSequence {
            mockRequestPasscode(passcodeRequest)(Left(Error("")))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a Bad request (400) response with code other than BAD_EMAIL_REQUEST " in {
          val json = Json.toJson(ErrorResponse("RANDOM_MESSAGE", "some random message"))

          inSequence {
            mockRequestPasscode(passcodeRequest)(Right(HttpResponse(BAD_REQUEST, json, emptyHeaders)))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-201 " in {
          val authenticatedRequest: AuthenticatedRequest[AnyContentAsEmpty.type]          = AuthenticatedRequest(
            new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "cy")), messagesApi)
          )
          implicit val requestWithSession: RequestWithSessionData[AnyContentAsEmpty.type] =
            RequestWithSessionData(authenticatedRequest, session)
          val passcodeRequest                                                             =
            PasscodeRequest(userSelectedEmail.emailAddress, serviceNameWelsh, Language.Welsh)

          inSequence {
            mockRequestPasscode(passcodeRequest)(Right(HttpResponse(OK, "", emptyHeaders)))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  401 (unauthorized)" in {
          inSequence {
            mockRequestPasscode(passcodeRequest)(Right(HttpResponse(UNAUTHORIZED, "", emptyHeaders)))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  502 (Bad Gateway)" in {
          val authenticatedRequest: AuthenticatedRequest[AnyContentAsEmpty.type]          = AuthenticatedRequest(
            new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "cy")), messagesApi)
          )
          implicit val requestWithSession: RequestWithSessionData[AnyContentAsEmpty.type] =
            RequestWithSessionData(authenticatedRequest, session)
          val passcodeRequest                                                             =
            PasscodeRequest(userSelectedEmail.emailAddress, serviceNameWelsh, Language.Welsh)

          inSequence {
            mockRequestPasscode(passcodeRequest)(Right(HttpResponse(BAD_GATEWAY, "", emptyHeaders)))
            mockSendAuditEvent(expectedAuditEvent)
          }
          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return Error Response" when {

        "http response came back with status 403 (Forbidden)" in {
          val userSelectedEmail  = UserSelectedEmail(EmailType.GGEmail, EmailAddress("max_emails_exceeded@email.com"))
          val passcodeRequest    = PasscodeRequest(userSelectedEmail.emailAddress, serviceNameEnglish, Language.English)
          val errorResponse      = ErrorResponse("MAX_EMAILS_EXCEEDED", "Too many emails or email addresses")
          val expectedAuditEvent = SubmitEmailAddressVerificationRequest(
            requestWithSession.sessionData.loginData.ggCredId,
            taxCheckCode,
            userSelectedEmail.emailAddress,
            userSelectedEmail.emailType,
            Some(MaximumNumberOfEmailsExceeded)
          )

          inSequence {
            mockRequestPasscode(passcodeRequest)(
              Right(HttpResponse(FORBIDDEN, Json.toJson(errorResponse), emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe Right(MaximumNumberOfEmailsExceeded)
        }

        "http response came back with status 409 (Conflict)" in {
          val authenticatedRequest: AuthenticatedRequest[AnyContentAsEmpty.type]          = AuthenticatedRequest(
            new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "cy")), messagesApi)
          )
          implicit val requestWithSession: RequestWithSessionData[AnyContentAsEmpty.type] =
            RequestWithSessionData(authenticatedRequest, session)
          val userSelectedEmail                                                           = UserSelectedEmail(EmailType.GGEmail, EmailAddress("email_verified_already@email.com"))
          val passcodeRequest                                                             = PasscodeRequest(userSelectedEmail.emailAddress, serviceNameWelsh, Language.Welsh)
          val errorResponse                                                               = ErrorResponse("EMAIL_VERIFIED_ALREADY", "Email has already been verified")
          val expectedAuditEvent                                                          = SubmitEmailAddressVerificationRequest(
            requestWithSession.sessionData.loginData.ggCredId,
            taxCheckCode,
            userSelectedEmail.emailAddress,
            userSelectedEmail.emailType,
            Some(EmailAddressAlreadyVerified)
          )

          inSequence {
            mockRequestPasscode(passcodeRequest)(
              Right(HttpResponse(CONFLICT, Json.toJson(errorResponse), emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe Right(EmailAddressAlreadyVerified)
        }

        "http response came back with status Bad request (400) and error response code is BAD_EMAIL_REQUEST" in {
          val userSelectedEmail  = UserSelectedEmail(EmailType.GGEmail, EmailAddress("bad_email_request@email.com"))
          val passcodeRequest    = PasscodeRequest(userSelectedEmail.emailAddress, serviceNameEnglish, Language.English)
          val errorResponse      =
            ErrorResponse("BAD_EMAIL_REQUEST", "email-verification had a problem, sendEmail returned bad request")
          val expectedAuditEvent = SubmitEmailAddressVerificationRequest(
            requestWithSession.sessionData.loginData.ggCredId,
            taxCheckCode,
            userSelectedEmail.emailAddress,
            userSelectedEmail.emailType,
            Some(BadEmailAddress)
          )

          inSequence {
            mockRequestPasscode(passcodeRequest)(
              Right(HttpResponse(BAD_REQUEST, Json.toJson(errorResponse), emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe Right(BadEmailAddress)
        }
      }

      "return successfully" when {

        val expectedAuditEvent = SubmitEmailAddressVerificationRequest(
          requestWithSession.sessionData.loginData.ggCredId,
          taxCheckCode,
          userSelectedEmail.emailAddress,
          userSelectedEmail.emailType,
          Some(PasscodeSent)
        )

        "the passcode has been successfully requested , Authenticated request has English language" in {
          inSequence {
            mockRequestPasscode(passcodeRequest)(Right(HttpResponse(CREATED, "")))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe Right(PasscodeSent)
        }

        "the passcode has been successfully requested, Authenticated request has Welsh language" in {
          val authenticatedRequest: AuthenticatedRequest[AnyContentAsEmpty.type]          = AuthenticatedRequest(
            new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "cy")), messagesApi)
          )
          implicit val requestWithSession: RequestWithSessionData[AnyContentAsEmpty.type] =
            RequestWithSessionData(authenticatedRequest, session)
          val passcodeRequest                                                             =
            PasscodeRequest(userSelectedEmail.emailAddress, serviceNameWelsh, Language.Welsh)

          inSequence {
            mockRequestPasscode(passcodeRequest)(Right(HttpResponse(CREATED, "")))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.requestPasscode(userSelectedEmail)
          await(result.value) shouldBe Right(PasscodeSent)
        }
      }

    }

    "handling request to verify passcode" must {

      val passcode = Passcode("AB12345")

      "return a technical error" when {

        val expectedAuditEvent = SubmitEmailAddressVerificationPasscode(
          requestWithSession.sessionData.loginData.ggCredId,
          taxCheckCode,
          userSelectedEmail.emailAddress,
          userSelectedEmail.emailType,
          passcode,
          None
        )

        "the http call fails" in {
          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(Left(Error("")))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-201 " in {
          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(Right(HttpResponse(OK, "", emptyHeaders)))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  401 (unauthorized)" in {
          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(
              Right(HttpResponse(UNAUTHORIZED, "", emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return Error Response" when {

        "http Response came back with status 403 (Forbidden), too many attempts" in {
          val passcode           = Passcode("CCCCCC")
          val errorResponse      = ErrorResponse("MAX_PASSCODE_ATTEMPTS_EXCEEDED", "Too many attempts")
          val expectedAuditEvent = SubmitEmailAddressVerificationPasscode(
            requestWithSession.sessionData.loginData.ggCredId,
            taxCheckCode,
            userSelectedEmail.emailAddress,
            userSelectedEmail.emailType,
            passcode,
            Some(TooManyAttempts)
          )

          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(
              Right(HttpResponse(FORBIDDEN, Json.toJson(errorResponse), emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe Right(TooManyAttempts)

        }

        "http Response came back with status 404 (Not Found), with status = PASSCODE_NOT_FOUND, response should be passcode is expired" in {
          val passcode           = Passcode("DDDDDD")
          val errorResponse      = ErrorResponse("PASSCODE_NOT_FOUND", "Passcode not found")
          val expectedAuditEvent = SubmitEmailAddressVerificationPasscode(
            requestWithSession.sessionData.loginData.ggCredId,
            taxCheckCode,
            userSelectedEmail.emailAddress,
            userSelectedEmail.emailType,
            passcode,
            Some(Expired)
          )

          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(
              Right(HttpResponse(NOT_FOUND, Json.toJson(errorResponse), emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe Right(Expired)

        }

        "http Response came back with status 404 (Not Found), with status = PASSCODE_MISMATCH, response should be No Match" in {
          val passcode           = Passcode("FFFFFF")
          val errorResponse      = ErrorResponse("PASSCODE_MISMATCH", "Passcode mismatch")
          val expectedAuditEvent = SubmitEmailAddressVerificationPasscode(
            requestWithSession.sessionData.loginData.ggCredId,
            taxCheckCode,
            userSelectedEmail.emailAddress,
            userSelectedEmail.emailType,
            passcode,
            Some(NoMatch)
          )

          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(
              Right(HttpResponse(NOT_FOUND, Json.toJson(errorResponse), emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe Right(NoMatch)

        }

      }

      "return success" when {

        val expectedAuditEvent = SubmitEmailAddressVerificationPasscode(
          requestWithSession.sessionData.loginData.ggCredId,
          taxCheckCode,
          userSelectedEmail.emailAddress,
          userSelectedEmail.emailType,
          passcode,
          Some(Match)
        )

        "http response came back with status 201" in {
          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(Right(HttpResponse(CREATED, "", emptyHeaders)))
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe Right(Match)

        }

        "http response came back with status 204 (NO_CONTENT)" in {
          inSequence {
            mockVerifyPasscode(passcode, userSelectedEmail.emailAddress)(
              Right(HttpResponse(NO_CONTENT, "", emptyHeaders))
            )
            mockSendAuditEvent(expectedAuditEvent)
          }

          val result = service.verifyPasscode(passcode, userSelectedEmail)
          await(result.value) shouldBe Right(Match)

        }
      }

    }

  }

}
