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
import cats.instances.future._
import com.google.inject.{ImplementedBy, Inject}
import play.api.http.Status._
import play.api.libs.json.{JsValue, Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.connectors.EmailVerificationConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.{SubmitEmailAddressVerificationPasscode, SubmitEmailAddressVerificationRequest}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult._
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeVerificationResult._
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Language, Passcode, PasscodeRequest, PasscodeRequestResult, PasscodeVerificationRequest, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.services.EmailVerificationService.ErrorResponse
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}
import uk.gov.hmrc.hecapplicantfrontend.util.HttpResponseOps._

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[EmailVerificationServiceImpl])
trait EmailVerificationService {

  def requestPasscode(
    userSelectedEmail: UserSelectedEmail
  )(implicit
    hc: HeaderCarrier,
    r: RequestWithSessionData[_]
  ): EitherT[Future, Error, PasscodeRequestResult]

  def verifyPasscode(passcode: Passcode, userSelectedEmail: UserSelectedEmail)(implicit
    hc: HeaderCarrier,
    r: RequestWithSessionData[_]
  ): EitherT[Future, Error, PasscodeVerificationResult]

}

@Singleton
class EmailVerificationServiceImpl @Inject() (
  emailVerificationConnector: EmailVerificationConnector,
  auditService: AuditService
)(implicit
  ec: ExecutionContext
) extends EmailVerificationService {

  val BAD_EMAIL_REQUEST  = "BAD_EMAIL_REQUEST"
  val PASSCODE_NOT_FOUND = "PASSCODE_NOT_FOUND"
  val PASSCODE_MISMATCH  = "PASSCODE_MISMATCH"

  override def requestPasscode(
    userSelectedEmail: UserSelectedEmail
  )(implicit
    hc: HeaderCarrier,
    r: RequestWithSessionData[_]
  ): EitherT[Future, Error, PasscodeRequestResult] = {
    def auditEvent(result: Option[PasscodeRequestResult]): SubmitEmailAddressVerificationRequest =
      SubmitEmailAddressVerificationRequest(
        r.sessionData.loginData.ggCredId,
        r.sessionData.emailRequestedForTaxCheck
          .map(_.taxCheckCode)
          .getOrElse(sys.error("Could not find tax check code")),
        userSelectedEmail.emailAddress,
        userSelectedEmail.emailType,
        result
      )

    val serviceName = r.request.request.messages("emailVerification.passcodeEmail.serviceName")

    val result: EitherT[Future, Error, HttpResponse] = for {
      lang   <- EitherT.fromEither[Future](Language.fromRequest(r.request)).leftMap(Error(_))
      result <-
        emailVerificationConnector.requestPasscode(PasscodeRequest(userSelectedEmail.emailAddress, serviceName, lang))
    } yield result

    result
      .leftMap { e =>
        auditService.sendEvent(auditEvent(None))
        e
      }
      .subflatMap { response =>
        val result = response.status match {
          case CONFLICT    => Right(EmailAddressAlreadyVerified)
          case FORBIDDEN   => Right(MaximumNumberOfEmailsExceeded)
          case CREATED     => Right(PasscodeSent)
          case BAD_REQUEST => evalRequestPasscodeBadRequest(response)
          case _           =>
            errorMessage(response.status)
        }
        auditService.sendEvent(auditEvent(result.toOption))
        result
      }

  }

  override def verifyPasscode(passcode: Passcode, userSelectedEmail: UserSelectedEmail)(implicit
    hc: HeaderCarrier,
    r: RequestWithSessionData[_]
  ): EitherT[Future, Error, PasscodeVerificationResult] = {
    def auditEvent(result: Option[PasscodeVerificationResult]): SubmitEmailAddressVerificationPasscode =
      SubmitEmailAddressVerificationPasscode(
        r.sessionData.loginData.ggCredId,
        r.sessionData.emailRequestedForTaxCheck
          .map(_.taxCheckCode)
          .getOrElse(sys.error("Could not find tax check code")),
        userSelectedEmail.emailAddress,
        userSelectedEmail.emailType,
        passcode,
        result
      )

    emailVerificationConnector
      .verifyPasscode(PasscodeVerificationRequest(passcode, userSelectedEmail.emailAddress))
      .leftMap { e =>
        auditService.sendEvent(auditEvent(None))
        e
      }
      .subflatMap { response =>
        val result = response.status match {
          case FORBIDDEN            => Right(TooManyAttempts)
          case NOT_FOUND            => evalVerifyPasscodeNotFound(response)
          case CREATED | NO_CONTENT => Right(Match)
          case _                    =>
            errorMessage(response.status)
        }
        auditService.sendEvent(auditEvent(result.toOption))
        result
      }

  }

  private def evalRequestPasscodeBadRequest(response: HttpResponse) = response.parseJSON[ErrorResponse] match {
    case Right(ErrorResponse(BAD_EMAIL_REQUEST, _)) => Right(BadEmailAddress)
    case _                                          => errorMessage(response.status)
  }

  private def evalVerifyPasscodeNotFound(response: HttpResponse) = response.parseJSON[ErrorResponse] match {
    case Right(ErrorResponse(PASSCODE_NOT_FOUND, _)) => Right(Expired)
    case Right(ErrorResponse(PASSCODE_MISMATCH, _))  => Right(NoMatch)
    case _                                           => errorMessage(response.status)
  }

  private def errorMessage(status: Int) = Left(
    Error(s"Call to verifyPasscode came back with status $status")
  )

}

object EmailVerificationService {
  final case class ErrorResponse(code: String, message: String)

  object ErrorResponse {

    implicit val format: OFormat[ErrorResponse] = Json.format[ErrorResponse]

    implicit class ErrorResponseOps(private val e: ErrorResponse) extends AnyVal {

      def json: JsValue = Json.toJson(e)

    }

  }
}
