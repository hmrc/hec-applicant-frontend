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
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchFailure.{EnrolmentCTUTRCompanyMatchFailure, EnterCTUTRCompanyMatchFailure}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchSuccess.{EnrolmentCTUTRCompanyMatchSuccess, EnterCTUTRCompanyMatchSuccess}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.{TaxCheckCodesDisplayed, TaxCheckExit}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId, NINO}

import java.time.{LocalDate, ZoneId, ZonedDateTime}

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

  "TaxCheckExit" must afterWord("have the correct JSON for") {

    def test(auditEvent: TaxCheckExit, expectedJson: JsValue) = {
      auditEvent.auditType       shouldBe "TaxCheckExit"
      auditEvent.transactionName shouldBe "tax-check-exit"
      Json.toJson(auditEvent)    shouldBe expectedJson
    }

    val session = IndividualHECSession(
      IndividualLoginData(
        GGCredId("credId"),
        NINO("nino"),
        None,
        Name("First", "Last"),
        DateOfBirth(LocalDate.of(2000, 1, 2)),
        None
      ),
      IndividualRetrievedJourneyData(None),
      IndividualUserAnswers.empty,
      None,
      Some(ZonedDateTime.of(2021, 12, 17, 16, 32, 33, 368000000, ZoneId.of("Europe/London"))),
      List.empty,
      hasConfirmedDetails = true,
      None,
      isEmailRequested = false,
      None
    )

    val sessionJson =
      """
        |{
        |  "loginData" : {
        |        "ggCredId" : "credId",
        |        "nino" : "nino",
        |        "name" : {
        |            "firstName" : "First",
        |            "lastName" : "Last"
        |        },
        |        "dateOfBirth" : "20000102"
        |    },
        |    "retrievedJourneyData" : {},
        |    "userAnswers" : {
        |        "type" : "Incomplete"
        |    },
        |    "taxCheckStartDateTime" : "2021-12-17T16:32:33.368Z[Europe/London]",
        |    "unexpiredTaxChecks" : [],
        |    "hasConfirmedDetails" : true,
        |    "isEmailRequested": false,
        |    "type" : "Individual"
        |}
        |""".stripMargin

    "SAUTRNotFound" in {
      test(
        TaxCheckExit.SAUTRNotFound(session),
        Json.parse(
          s"""
            |{
            |  "serviceExitReason": "SAUTRNotFound",
            |  "serviceExitDescription": "SA UTR not found for the Applicant's NINO",
            |  "taxCheckSessionData": $sessionJson
            |}
            |""".stripMargin
        )
      )
    }

    "SANoNoticeToFileOrTaxReturn" in {
      test(
        TaxCheckExit.SANoNoticeToFileOrTaxReturn(session),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "SANoNoticeToFileOrTaxReturn",
             |  "serviceExitDescription": "For relevant income tax year, Self Assessment Notice to File not found, Self Assessment Tax Return not found",
             |  "taxCheckSessionData": $sessionJson
             |}
             |""".stripMargin
        )
      )
    }

    "CTEnteredCTUTRNotMatchingBlocked" in {
      test(
        TaxCheckExit.CTEnteredCTUTRNotMatchingBlocked(session),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "CTEnteredCTUTRNotMatchingBlocked",
             |  "serviceExitDescription": "Applicant has made repeated attempts to provide a matching CT UTR. Attempts limit reached, so Applicant temporarily blocked from making an Application for that CRN",
             |  "taxCheckSessionData": $sessionJson
             |}
             |""".stripMargin
        )
      )
    }

    "CTNoNoticeToFileOrTaxReturn" in {
      test(
        TaxCheckExit.CTNoNoticeToFileOrTaxReturn(session),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "CTNoNoticeToFileOrTaxReturn",
             |  "serviceExitDescription": "For relevant accounting period, Corporation Tax Notice to File not found, Corporation Tax Return not found",
             |  "taxCheckSessionData": $sessionJson
             |}
             |""".stripMargin
        )
      )
    }

    "CTNoAccountingPeriodNotRecentlyStartedTrading" in {
      test(
        TaxCheckExit.CTNoAccountingPeriodNotRecentlyStartedTrading(session),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "CTNoAccountingPeriodNotRecentlyStartedTrading",
             |  "serviceExitDescription": "No relevant accounting period was found on tax summary record for the lookback period, and the Applicant's Company has not recently started trading",
             |  "taxCheckSessionData": $sessionJson
             |}
             |""".stripMargin
        )
      )
    }

    "AllowedTaxChecksExceeded" in {
      test(
        TaxCheckExit.AllowedTaxChecksExceeded(session),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "AllowedTaxChecksExceeded",
             |  "serviceExitDescription": "Attempted tax check for Licence Type exceeded number of permitted existing tax check codes, for a particular Applicant",
             |  "taxCheckSessionData": $sessionJson
             |}
             |""".stripMargin
        )
      )
    }

  }

}
