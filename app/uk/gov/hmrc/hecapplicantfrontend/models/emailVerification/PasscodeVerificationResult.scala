package uk.gov.hmrc.hecapplicantfrontend.models.emailVerification

sealed trait PasscodeVerificationResult extends Product with Serializable

object PasscodeVerificationResult {

  case object Match extends PasscodeVerificationResult

  case object NoMatch extends PasscodeVerificationResult

  case object Expired extends PasscodeVerificationResult

  case object TooManyAttempts extends PasscodeVerificationResult

}
