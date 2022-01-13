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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.json.{JsObject, JsString, Json, OWrites, Writes}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatch.{CTUTRType, MatchResult}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId}

sealed trait AuditEvent {

  val auditType: String

  val transactionName: String

}

object AuditEvent {

  final case class TaxCheckCodesDisplayed(
    ggCredId: GGCredId,
    taxCheckCodes: List[HECTaxCheckCode]
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
        Map("ctutrType" -> JsString(c.ctutrType.toString), "matchResult" -> JsString(c.matchResult.toString))
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
      tooManyAttempts: Boolean
    ) extends CompanyMatchFailure {
      override val ctutrType: CTUTRType = CTUTRType.Submitted
    }

    final case class EnrolmentCTUTRCompanyMatchFailure(
      companyRegistrationNumber: CRN,
      hmrcCTUTR: CTUTR,
      enrolmentCTUTR: CTUTR
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
      hmrcCTUTR: CTUTR
    ) extends CompanyMatchSuccess {
      override val ctutrType: CTUTRType = CTUTRType.Submitted
    }

    final case class EnrolmentCTUTRCompanyMatchSuccess(
      companyRegistrationNumber: CRN,
      hmrcCTUTR: CTUTR,
      enrolmentCTUTR: CTUTR
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

    val serviceExitReason: String

    val serviceExitDescription: String

    val taxCheckSessionData: HECSession
  }

  object TaxCheckExit {

    implicit def writes[T <: TaxCheckExit]: OWrites[T] = OWrites { t: T =>
      Json.obj(
        "serviceExitReason"      -> JsString(t.serviceExitReason),
        "serviceExitDescription" -> JsString(t.serviceExitDescription),
        "taxCheckSessionData"    -> Json.toJson(t.taxCheckSessionData)
      )
    }

    final case class SAUTRNotFound(taxCheckSessionData: HECSession) extends TaxCheckExit {
      val serviceExitReason: String = "SAUTRNotFound"

      val serviceExitDescription: String = "SA UTR not found for the Applicant's NINO."
    }

    final case class SANoNoticeToFileOrTaxReturn(taxCheckSessionData: HECSession) extends TaxCheckExit {
      val serviceExitReason: String = "SANoNoticeToFileOrTaxReturn"

      val serviceExitDescription: String =
        "For relevant income tax year, Self Assessment Notice to File not found, Self Assessment Tax Return not found."
    }

    final case class CTEnteredCTUTRNotMatchingBlocked(taxCheckSessionData: HECSession) extends TaxCheckExit {
      val serviceExitReason: String = "CTEnteredCTUTRNotMatchingBlocked"

      val serviceExitDescription: String =
        "Applicant has made repeated attempts to provide a matching CT UTR. Attempts limit reached, " +
          "so Applicant temporarily blocked from making an Application for that CRN."
    }

    final case class CTNoNoticeToFileOrTaxReturn(taxCheckSessionData: HECSession) extends TaxCheckExit {
      val serviceExitReason: String = "CTNoNoticeToFileOrTaxReturn"

      val serviceExitDescription: String =
        "For relevant accounting period, Corporation Tax Notice to File not found, Corporation Tax Return not found."
    }

    final case class CTNoAccountingPeriodNotRecentlyStartedTrading(taxCheckSessionData: HECSession)
        extends TaxCheckExit {
      val serviceExitReason: String = "CTNoAccountingPeriodNotRecentlyStartedTrading"

      val serviceExitDescription: String =
        "No relevant accounting period was found on tax summary record for the lookback period, and the " +
          "Applicant's Company has not recently started trading."
    }

    final case class AllowedTaxChecksExceeded(taxCheckSessionData: HECSession) extends TaxCheckExit {
      val serviceExitReason: String = "AllowedTaxChecksExceeded"

      val serviceExitDescription: String =
        "Attempted tax check for Licence Type exceeded number of permitted existing tax check codes, " +
          "for a particular Applicant."
    }

  }

}
