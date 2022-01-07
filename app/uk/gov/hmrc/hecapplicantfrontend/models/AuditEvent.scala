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

import play.api.libs.json.{JsString, Json, OWrites, Writes}
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

  sealed trait CompanyMatchFailure extends AuditEvent {
    val auditType: String = "CompanyMatchFailure"

    val transactionName: String = "company-match-failure"
  }

  object CompanyMatchFailure {

    final case class EnterCTUTRCompanyMatchFailure(
      companyRegistrationNumber: CRN,
      submittedCTUTR: CTUTR,
      submittedCTUTRStandardised: CTUTR,
      hmrcCTUTR: CTUTR,
      tooManyAttempts: Boolean
    ) extends CompanyMatchFailure

    final case class EnrolmentCTUTRCompanyMatchFailure(
      companyRegistrationNumber: CRN,
      hmrcCTUTR: CTUTR,
      enrolmentCTUTR: CTUTR
    ) extends CompanyMatchFailure

    implicit val enterCTUTRMatchFailureWrites: OWrites[EnterCTUTRCompanyMatchFailure] = Json.writes

    implicit val enrolmentCTUTRMatchFailureWrites: OWrites[EnrolmentCTUTRCompanyMatchFailure] = Json.writes

  }

  sealed trait CompanyMatchSuccess extends AuditEvent {
    val auditType: String = "CompanyMatchSuccess"

    val transactionName: String = "company-match-success"
  }

  object CompanyMatchSuccess {

    final case class EnterCTUTRCompanyMatchSuccess(
      companyRegistrationNumber: CRN,
      submittedCTUTR: CTUTR,
      submittedCTUTRStandardised: CTUTR,
      hmrcCTUTR: CTUTR
    ) extends CompanyMatchSuccess

    final case class EnrolmentCTUTRCompanyMatchSuccess(
      companyRegistrationNumber: CRN,
      hmrcCTUTR: CTUTR,
      enrolmentCTUTR: CTUTR
    ) extends CompanyMatchSuccess

    implicit val enterCTUTRMatchSuccessWrites: OWrites[EnterCTUTRCompanyMatchSuccess] = Json.writes

    implicit val enrolmentCTUTRMatchSuccessWrites: OWrites[EnrolmentCTUTRCompanyMatchSuccess] = Json.writes

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
