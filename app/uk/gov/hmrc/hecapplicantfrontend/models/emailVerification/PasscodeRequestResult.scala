package uk.gov.hmrc.hecapplicantfrontend.models.emailVerification

sealed trait PasscodeRequestResult extends Product with Serializable

object PasscodeRequestResult {

  case object PasscodeSent extends PasscodeRequestResult

  case object EmailAddressAlreadyVerified extends PasscodeRequestResult

  case object MaximumNumberOfEmailsExceeded extends PasscodeRequestResult

  case object BadEmailAddress extends PasscodeRequestResult

}
