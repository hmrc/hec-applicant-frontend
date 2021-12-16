package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthenticatedRequest
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.Future

trait EmailVerificationService {

  def requestPasscode(
    emailAddress: EmailAddress
  )(implicit hc: HeaderCarrier, r: AuthenticatedRequest[_]): EitherT[Future, Error, PasscodeRequestResult]

  def verifyPasscode(passcode: Passcode, emailAddress: EmailAddress)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, PasscodeVerificationResult]

}
