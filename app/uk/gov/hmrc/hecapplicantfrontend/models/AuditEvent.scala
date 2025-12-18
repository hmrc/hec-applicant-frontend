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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.json._
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed.AuthenticationDetails
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatch.{CTUTRType, MatchResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult._
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId}

sealed trait AuditEvent {

  val auditType: String

  val transactionName: String

}

object AuditEvent {

  final case class TaxCheckCodesDisplayed(
    ggCredId: GGCredId,
    taxCheckCodes: List[HECTaxCheckCode],
    languagePreference: Language
  ) extends AuditEvent {
    val auditType: String = "TaxCheckCodesDisplayed"

    val transactionName: String = "tax-check-codes-displayed"
  }

  object TaxCheckCodesDisplayed {

    implicit val writes: Writes[TaxCheckCodesDisplayed] = Json.writes

  }

  sealed trait CompanyMatch extends AuditEvent {
    val auditType: String = "CompanyMatch"

    val transactionName: String = "company-match"

    val ctutrType: CTUTRType

    val matchResult: MatchResult

    val languagePreference: Language

    val ggCredId: GGCredId

  }

  object CompanyMatch {

    sealed trait CTUTRType extends Product with Serializable

    object CTUTRType {

      case object Enrolment extends CTUTRType

      case object Submitted extends CTUTRType

    }

    sealed trait MatchResult extends Product with Serializable

    object MatchResult {

      case object Match extends MatchResult

      case object NoMatch extends MatchResult

    }

    def companyMatchWrites[A <: CompanyMatch](writes: OWrites[A]): OWrites[A] = OWrites { c =>
      writes.writes(c) ++ JsObject(
        Map(
          "ctutrType"          -> JsString(c.ctutrType.toString),
          "matchResult"        -> JsString(c.matchResult.toString),
          "languagePreference" -> Json.toJson(c.languagePreference),
          "ggCredId"           -> Json.toJson(c.ggCredId)
        )
      )
    }

  }

  sealed trait CompanyMatchFailure extends CompanyMatch {
    override val matchResult: MatchResult = MatchResult.NoMatch
  }

  object CompanyMatchFailure {

    final case class EnterCTUTRCompanyMatchFailure(
      companyRegistrationNumber: CRN,
      submittedCTUTR: CTUTR,
      submittedCTUTRStandardised: CTUTR,
      hmrcCTUTR: CTUTR,
      tooManyAttempts: Boolean,
      languagePreference: Language,
      ggCredId: GGCredId
    ) extends CompanyMatchFailure {
      override val ctutrType: CTUTRType = CTUTRType.Submitted
    }

    final case class EnrolmentCTUTRCompanyMatchFailure(
      companyRegistrationNumber: CRN,
      hmrcCTUTR: CTUTR,
      enrolmentCTUTR: CTUTR,
      languagePreference: Language,
      ggCredId: GGCredId
    ) extends CompanyMatchFailure {
      override val ctutrType: CTUTRType = CTUTRType.Enrolment
    }

    implicit val enterCTUTRMatchFailureWrites: OWrites[EnterCTUTRCompanyMatchFailure] =
      CompanyMatch.companyMatchWrites(Json.writes[EnterCTUTRCompanyMatchFailure])

    implicit val enrolmentCTUTRMatchFailureWrites: OWrites[EnrolmentCTUTRCompanyMatchFailure] =
      CompanyMatch.companyMatchWrites(Json.writes[EnrolmentCTUTRCompanyMatchFailure])

  }

  sealed trait CompanyMatchSuccess extends CompanyMatch {
    override val matchResult: MatchResult = MatchResult.Match
  }

  object CompanyMatchSuccess {

    final case class EnterCTUTRCompanyMatchSuccess(
      companyRegistrationNumber: CRN,
      submittedCTUTR: CTUTR,
      submittedCTUTRStandardised: CTUTR,
      hmrcCTUTR: CTUTR,
      languagePreference: Language,
      ggCredId: GGCredId
    ) extends CompanyMatchSuccess {
      override val ctutrType: CTUTRType = CTUTRType.Submitted
    }

    final case class EnrolmentCTUTRCompanyMatchSuccess(
      companyRegistrationNumber: CRN,
      hmrcCTUTR: CTUTR,
      enrolmentCTUTR: CTUTR,
      languagePreference: Language,
      ggCredId: GGCredId
    ) extends CompanyMatchSuccess {
      override val ctutrType: CTUTRType = CTUTRType.Enrolment
    }

    implicit val enterCTUTRMatchSuccessWrites: OWrites[EnterCTUTRCompanyMatchSuccess] =
      CompanyMatch.companyMatchWrites(Json.writes[EnterCTUTRCompanyMatchSuccess])

    implicit val enrolmentCTUTRMatchSuccessWrites: OWrites[EnrolmentCTUTRCompanyMatchSuccess] =
      CompanyMatch.companyMatchWrites(Json.writes[EnrolmentCTUTRCompanyMatchSuccess])

  }

  sealed trait TaxCheckExit extends AuditEvent {
    val auditType: String = "TaxCheckExit"

    val transactionName: String = "tax-check-exit"

    val source: String = "Digital"

    val serviceExitReason: String

    val serviceExitDescription: String

    val taxCheckSessionData: HECSession

    val languagePreference: Language
  }

  object TaxCheckExit {

    implicit def writes[T <: TaxCheckExit]: OWrites[T] = OWrites { (t: T) =>
      Json.obj(
        "serviceExitReason"      -> JsString(t.serviceExitReason),
        "serviceExitDescription" -> JsString(t.serviceExitDescription),
        "source"                 -> JsString(t.source),
        "taxCheckSessionData"    -> Json.toJson(t.taxCheckSessionData),
        "languagePreference"     -> Json.toJson(t.languagePreference)
      )
    }

    final case class SAUTRNotFound(taxCheckSessionData: HECSession, languagePreference: Language) extends TaxCheckExit {
      val serviceExitReason: String = "SAUTRNotFound"

      val serviceExitDescription: String = "SA UTR not found for the Applicant's NINO."
    }

    final case class SANoNoticeToFileOrTaxReturn(taxCheckSessionData: HECSession, languagePreference: Language)
        extends TaxCheckExit {
      val serviceExitReason: String = "SANoNoticeToFileOrTaxReturn"

      val serviceExitDescription: String =
        "For relevant income tax year, Self Assessment Notice to File not found, Self Assessment Tax Return not found."
    }

    final case class CTEnteredCTUTRNotMatchingBlocked(taxCheckSessionData: HECSession, languagePreference: Language)
        extends TaxCheckExit {
      val serviceExitReason: String = "CTEnteredCTUTRNotMatchingBlocked"

      val serviceExitDescription: String =
        "Applicant has made repeated attempts to provide a matching CT UTR. Attempts limit reached, " +
          "so Applicant temporarily blocked from making an Application for that CRN."
    }

    final case class CTNoNoticeToFileOrTaxReturn(taxCheckSessionData: HECSession, languagePreference: Language)
        extends TaxCheckExit {
      val serviceExitReason: String = "CTNoNoticeToFileOrTaxReturn"

      val serviceExitDescription: String =
        "For relevant accounting period, Corporation Tax Notice to File not found, Corporation Tax Return not found."
    }

    final case class CTNoAccountingPeriodNotRecentlyStartedTrading(
      taxCheckSessionData: HECSession,
      languagePreference: Language
    ) extends TaxCheckExit {
      val serviceExitReason: String = "CTNoAccountingPeriodNotRecentlyStartedTrading"

      val serviceExitDescription: String =
        "No relevant accounting period was found on tax summary record for the lookback period, and the " +
          "Applicant's Company has not recently started trading."
    }

    final case class AllowedTaxChecksExceeded(taxCheckSessionData: HECSession, languagePreference: Language)
        extends TaxCheckExit {
      val serviceExitReason: String = "AllowedTaxChecksExceeded"

      val serviceExitDescription: String =
        "Attempted tax check for Licence Type exceeded number of permitted existing tax check codes, " +
          "for a particular Applicant."
    }

  }

  sealed trait EmailAuditEvent extends AuditEvent {
    val ggCredId: GGCredId
    val taxCheckCode: HECTaxCheckCode
    val emailAddress: EmailAddress
    val emailSource: EmailType
    val languagePreference: Language
  }

  object EmailAuditEvent {

    private val emailTypeWrites: Writes[EmailType] = Writes {
      case EmailType.GGEmail        => JsString("GovernmentGateway")
      case EmailType.DifferentEmail => JsString("Submitted")
    }

    def emailAuditEventJson(e: EmailAuditEvent): JsObject =
      JsObject(
        Map(
          "ggCredId"           -> Json.toJson(e.ggCredId),
          "taxCheckCode"       -> Json.toJson(e.taxCheckCode),
          "emailAddress"       -> Json.toJson(e.emailAddress),
          "emailSource"        -> emailTypeWrites.writes(e.emailSource),
          "languagePreference" -> Json.toJson(e.languagePreference)
        )
      )
  }

  final case class SubmitEmailAddressVerificationRequest(
    ggCredId: GGCredId,
    taxCheckCode: HECTaxCheckCode,
    emailAddress: EmailAddress,
    emailSource: EmailType,
    result: Option[PasscodeRequestResult],
    languagePreference: Language
  ) extends EmailAuditEvent {

    override val auditType: String = "SubmitEmailAddressVerificationRequest"

    override val transactionName: String = "submit-email-address-verification-request"
  }

  object SubmitEmailAddressVerificationRequest {

    private def resultJson(result: Option[PasscodeRequestResult]): JsString = result match {
      case Some(PasscodeSent)                                                 => JsString("Success")
      case Some(EmailAddressAlreadyVerified)                                  => JsString("AlreadyVerified")
      case Some(MaximumNumberOfEmailsExceeded) | Some(BadEmailAddress) | None => JsString("Failure")
    }

    private def failureReason(result: Option[PasscodeRequestResult]): Option[String] = result match {
      case Some(PasscodeSent) | Some(EmailAddressAlreadyVerified) => None
      case Some(BadEmailAddress)                                  => Some("VerificationPasscodeEmailFailed")
      case Some(MaximumNumberOfEmailsExceeded)                    => Some("AttemptsToVerifyEmailAddressExceeded")
      case _                                                      => Some("TechnicalError")
    }

    implicit val writes: OWrites[SubmitEmailAddressVerificationRequest] =
      OWrites { s =>
        val resultKeyValue = "result" -> resultJson(s.result)
        val json           = EmailAuditEvent.emailAuditEventJson(s) + resultKeyValue
        failureReason(s.result).fold(json)(f => json + ("failureReason" -> JsString(f)))
      }

  }

  final case class SubmitEmailAddressVerificationPasscode(
    ggCredId: GGCredId,
    taxCheckCode: HECTaxCheckCode,
    emailAddress: EmailAddress,
    emailSource: EmailType,
    verificationPasscode: Passcode,
    result: Option[PasscodeVerificationResult],
    languagePreference: Language
  ) extends EmailAuditEvent {

    override val auditType: String = "SubmitEmailAddressVerificationPasscode"

    override val transactionName: String = "submit-email-address-verification-passcode"
  }

  object SubmitEmailAddressVerificationPasscode {

    private def resultJson(result: Option[PasscodeVerificationResult]): JsString = result match {
      case Some(PasscodeVerificationResult.Match) => JsString("Success")
      case _                                      => JsString("Failure")
    }

    private def failureReason(result: Option[PasscodeVerificationResult]): Option[String] = result match {
      case Some(PasscodeVerificationResult.Match)           => None
      case Some(PasscodeVerificationResult.TooManyAttempts) => Some("MaximumPasscodeVerificationAttemptsExceeded")
      case Some(PasscodeVerificationResult.Expired)         => Some("PasscodeNotFoundExpired")
      case Some(PasscodeVerificationResult.NoMatch)         => Some("PasscodeMismatch")
      case None                                             => Some("TechnicalError")
    }

    implicit val writes: OWrites[SubmitEmailAddressVerificationPasscode] =
      OWrites { s =>
        val json = EmailAuditEvent.emailAuditEventJson(s) ++ JsObject(
          Map(
            "verificationPasscode" -> JsString(s.verificationPasscode.value),
            "result"               -> resultJson(s.result)
          )
        )
        failureReason(s.result).fold(json)(f => json + ("failureReason" -> JsString(f)))
      }

  }

  final case class SendTaxCheckCodeNotificationEmail(
    ggCredId: GGCredId,
    taxCheckCode: HECTaxCheckCode,
    emailAddress: EmailAddress,
    emailSource: EmailType,
    templateId: String,
    result: Option[EmailSendResult],
    languagePreference: Language
  ) extends EmailAuditEvent {

    override val auditType: String = "SendTaxCheckCodeNotificationEmail"

    override val transactionName: String = "send-tax-check-code-notification-email"
  }

  object SendTaxCheckCodeNotificationEmail {

    private def resultJson(result: Option[EmailSendResult]): JsString = result match {
      case Some(EmailSendResult.EmailSent)               => JsString("Success")
      case Some(EmailSendResult.EmailSentFailure) | None => JsString("Failure")
    }

    implicit val writes: OWrites[SendTaxCheckCodeNotificationEmail] =
      OWrites { s =>
        EmailAuditEvent.emailAuditEventJson(s) ++ JsObject(
          Map(
            "emailTemplateIdentifier" -> JsString(s.templateId),
            "result"                  -> resultJson(s.result)
          )
        )
      }

  }

  final case class ApplicantServiceStartEndPointAccessed(
    authenticationStatus: AuthenticationStatus,
    redirectionUrl: Option[String],
    authenticationDetail: Option[AuthenticationDetails]
  ) extends AuditEvent {
    override val auditType: String       = "ApplicantServiceStartEndPointAccessed"
    override val transactionName: String = "applicant-service-start-end-point-accessed"
  }

  object ApplicantServiceStartEndPointAccessed {

    final case class AuthenticationDetails(
      authenticationProvider: String,
      authenticationProviderCredId: String,
      ggAffinityGroup: Option[AffinityGroup],
      entityType: Option[EntityType],
      confidenceLevel: ConfidenceLevel
    )

    object AuthenticationDetails {

      implicit val writes: OWrites[AuthenticationDetails] = {
        implicit val confidenceLevelWrites: Writes[ConfidenceLevel] = Writes(cl => JsString(s"CL${cl.level}"))
        Json.writes
      }
    }

    implicit val writes: OWrites[ApplicantServiceStartEndPointAccessed] = Json.writes

  }

}
