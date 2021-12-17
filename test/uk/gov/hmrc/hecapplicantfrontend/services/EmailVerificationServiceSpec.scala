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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.Status.{BAD_GATEWAY, FORBIDDEN}
import play.api.libs.json.Json
import play.api.mvc.MessagesRequest
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.connectors.EmailVerificationConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.ControllerSpec
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthenticatedRequest
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult._
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeVerificationResult._
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Language, Passcode, PasscodeRequest, PasscodeVerificationRequest}
import uk.gov.hmrc.hecapplicantfrontend.services.EmailVerificationService.ErrorResponse
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global

class EmailVerificationServiceSpec extends AnyWordSpec with Matchers with MockFactory with ControllerSpec {

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

  val service                       = new EmailVerificationServiceImpl(mockEmailVerificationConnector)
  val emptyHeaders                  = Map.empty[String, Seq[String]]
  implicit val hc: HeaderCarrier    = HeaderCarrier()
  implicit val authenticatedRequest = AuthenticatedRequest(new MessagesRequest(FakeRequest(), messagesApi))

  "EmailVerificationServiceSpec" when {
    val emailAddress = EmailAddress("user@test.com")

    "handling request to requestPasscode" must {
      val passcodeRequest = PasscodeRequest(emailAddress, "hec", Language.English)

      "return a technical  error" when {

        "the http call fails" in {
          mockRequestPasscode(passcodeRequest)(Left(Error("")))

          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a Bad request (400) response with code other than BAD_EMAIL_REQUEST " in {
          val json = Json.toJson(ErrorResponse("RANDOM_MESSAGE", "some random message"))
          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(BAD_REQUEST, json, emptyHeaders)))

          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-201 " in {

          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(OK, "", emptyHeaders)))

          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  401 (unauthorized)" in {
          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(UNAUTHORIZED, "", emptyHeaders)))
          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  502 (Bad Gateway)" in {
          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(BAD_GATEWAY, "", emptyHeaders)))
          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return Error Response" when {

        "http response came back with status 403 (Forbidden)" in {

          val emailAddress    = EmailAddress("max_emails_exceeded@email.com")
          val passcodeRequest = PasscodeRequest(emailAddress, "hec", Language.English)
          val errorResponse   = ErrorResponse("MAX_EMAILS_EXCEEDED", "Too many emails or email addresses")
          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(FORBIDDEN, Json.toJson(errorResponse), emptyHeaders)))

          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe Right(MaximumNumberOfEmailsExceeded)
        }

        "http response came back with status 409 (Conflict)" in {

          val emailAddress    = EmailAddress("email_verified_already@email.com")
          val passcodeRequest = PasscodeRequest(emailAddress, "hec", Language.English)
          val errorResponse   = ErrorResponse("EMAIL_VERIFIED_ALREADY", "Email has already been verified")
          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(CONFLICT, Json.toJson(errorResponse), emptyHeaders)))

          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe Right(EmailAddressAlreadyVerified)
        }

        "http response came back with status Bad request (400) and error response code is BAD_EMAIL_REQUEST" in {

          val emailAddress    = EmailAddress("bad_email_request@email.com")
          val passcodeRequest = PasscodeRequest(emailAddress, "hec", Language.English)
          val errorResponse   =
            ErrorResponse("BAD_EMAIL_REQUEST", "email-verification had a problem, sendEmail returned bad request")
          mockRequestPasscode(passcodeRequest)(
            Right(HttpResponse(BAD_REQUEST, Json.toJson(errorResponse), emptyHeaders))
          )

          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe Right(BadEmailAddress)
        }
      }

      "return successfully" when {

        "the passcode has been successfully requested " in {

          mockRequestPasscode(passcodeRequest)(Right(HttpResponse(CREATED, "")))
          val result = service.requestPasscode(emailAddress)
          await(result.value) shouldBe Right(PasscodeSent)
        }
      }

    }

    "handling request to verify passcode" must {

      val passcode = Passcode("AB12345")

      "return a technical error" when {

        "the http call fails" in {
          mockVerifyPasscode(passcode, emailAddress)(Left(Error("")))

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a non-201 " in {
          mockVerifyPasscode(passcode, emailAddress)(Right(HttpResponse(OK, "", emptyHeaders)))

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  401 (unauthorized)" in {
          mockVerifyPasscode(passcode, emailAddress)(Right(HttpResponse(UNAUTHORIZED, "", emptyHeaders)))

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http response came back with  204 (No COntent)" in {
          mockVerifyPasscode(passcode, emailAddress)(Right(HttpResponse(NO_CONTENT, "", emptyHeaders)))

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return Error Response" when {

        "http Response came back with status 403 (Forbidden), too many attempts" in {
          val passcode      = Passcode("CCCCCC")
          val errorResponse = ErrorResponse("MAX_PASSCODE_ATTEMPTS_EXCEEDED", "Too many attempts")
          mockVerifyPasscode(passcode, emailAddress)(
            Right(HttpResponse(FORBIDDEN, Json.toJson(errorResponse), emptyHeaders))
          )

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe Right(TooManyAttempts)

        }

        "http Response came back with status 404 (Not Found), with status = PASSCODE_NOT_FOUND, response should be passcode is expired" in {
          val passcode      = Passcode("DDDDDD")
          val errorResponse = ErrorResponse("PASSCODE_NOT_FOUND", "Passcode not found")
          mockVerifyPasscode(passcode, emailAddress)(
            Right(HttpResponse(NOT_FOUND, Json.toJson(errorResponse), emptyHeaders))
          )

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe Right(Expired)

        }

        "http Response came back with status 404 (Not Found), with status = PASSCODE_MISMATCH, response should be No Match" in {
          val passcode      = Passcode("FFFFFF")
          val errorResponse = ErrorResponse("PASSCODE_MISMATCH", "Passcode mismatch")
          mockVerifyPasscode(passcode, emailAddress)(
            Right(HttpResponse(NOT_FOUND, Json.toJson(errorResponse), emptyHeaders))
          )

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe Right(NoMatch)

        }

      }

      "return status 201" when {
        "passcode has been successfully verified" in {
          mockVerifyPasscode(passcode, emailAddress)(Right(HttpResponse(CREATED, "", emptyHeaders)))

          val result = service.verifyPasscode(passcode, emailAddress)
          await(result.value) shouldBe Right(Match)

        }
      }

    }

  }

}
