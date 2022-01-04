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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchFailure.{EnrolmentCTUTRCompanyMatchFailure, EnterCTUTRCompanyMatchFailure}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchSuccess.{EnrolmentCTUTRCompanyMatchSuccess, EnterCTUTRCompanyMatchSuccess}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckCodesDisplayed
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId}

class AuditEventSpec extends Matchers with AnyWordSpecLike {

  "TaxCheckCodesDisplayed" must {

    "have the correct JSON" in {
      val auditEvent = TaxCheckCodesDisplayed(
        GGCredId("ggCred"),
        List(
          HECTaxCheckCode("ABC"),
          HECTaxCheckCode("DEF")
        )
      )

      auditEvent.auditType       shouldBe "TaxCheckCodesDisplayed"
      auditEvent.transactionName shouldBe "tax-check-codes-displayed"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "ggCredId": "ggCred",
          |  "taxCheckCodes": [ "ABC", "DEF" ]
          |}
          |""".stripMargin
      )

    }

  }

  "EnterCTUTRCompanyMatchFailure" must {

    "have the correct JSON" in {
      val auditEvent = EnterCTUTRCompanyMatchFailure(
        CRN("12345678"),
        CTUTR("1111111111"),
        CTUTR("2222222222"),
        CTUTR("3333333333"),
        true
      )

      auditEvent.auditType       shouldBe "CompanyMatchFailure"
      auditEvent.transactionName shouldBe "company-match-failure"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "companyRegistrationNumber": "12345678",
          |  "submittedCTUTR": "1111111111",
          |  "submittedCTUTRStandardised": "2222222222",
          |  "hmrcCTUTR": "3333333333",
          |  "tooManyAttempts": true
          |}
          |""".stripMargin
      )
    }

  }

  "EnrolmentCTUTRCompanyMatchFailure" must {

    "have the correct JSON" in {
      val auditEvent = EnrolmentCTUTRCompanyMatchFailure(
        CRN("12345678"),
        CTUTR("3333333333"),
        CTUTR("4444444444")
      )

      auditEvent.auditType       shouldBe "CompanyMatchFailure"
      auditEvent.transactionName shouldBe "company-match-failure"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "companyRegistrationNumber": "12345678",
          |  "hmrcCTUTR": "3333333333",
          |  "enrolmentCTUTR": "4444444444"
          |}
          |""".stripMargin
      )
    }

  }

  "EnterCTUTRCompanyMatchSuccess" must {

    "have the correct JSON" in {
      val auditEvent = EnterCTUTRCompanyMatchSuccess(
        CRN("12345678"),
        CTUTR("1111111111"),
        CTUTR("2222222222"),
        CTUTR("3333333333")
      )

      auditEvent.auditType       shouldBe "CompanyMatchSuccess"
      auditEvent.transactionName shouldBe "company-match-success"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "companyRegistrationNumber": "12345678",
          |  "submittedCTUTR": "1111111111",
          |  "submittedCTUTRStandardised": "2222222222",
          |  "hmrcCTUTR": "3333333333"
          |}
          |""".stripMargin
      )
    }

  }

  "EnrolmentCTUTRCompanyMatchSuccess" must {

    "have the correct JSON" in {
      val auditEvent = EnrolmentCTUTRCompanyMatchSuccess(
        CRN("12345678"),
        CTUTR("3333333333"),
        CTUTR("4444444444")
      )

      auditEvent.auditType       shouldBe "CompanyMatchSuccess"
      auditEvent.transactionName shouldBe "company-match-success"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "companyRegistrationNumber": "12345678",
          |  "hmrcCTUTR": "3333333333",
          |  "enrolmentCTUTR": "4444444444"
          |}
          |""".stripMargin
      )
    }

  }

}
