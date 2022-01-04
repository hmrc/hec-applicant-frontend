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

import play.api.libs.json.{Json, OWrites, Writes}
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

}
