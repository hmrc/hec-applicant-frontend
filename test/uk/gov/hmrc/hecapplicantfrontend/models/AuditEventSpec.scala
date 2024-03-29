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

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.libs.json.{JsValue, Json}
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed.AuthenticationDetails
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchFailure.{EnrolmentCTUTRCompanyMatchFailure, EnterCTUTRCompanyMatchFailure}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.CompanyMatchSuccess.{EnrolmentCTUTRCompanyMatchSuccess, EnterCTUTRCompanyMatchSuccess}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.{ApplicantServiceStartEndPointAccessed, SendTaxCheckCodeNotificationEmail, SubmitEmailAddressVerificationPasscode, SubmitEmailAddressVerificationRequest, TaxCheckCodesDisplayed, TaxCheckExit}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
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
        ),
        Language.Welsh
      )

      auditEvent.auditType       shouldBe "TaxCheckCodesDisplayed"
      auditEvent.transactionName shouldBe "tax-check-codes-displayed"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "ggCredId": "ggCred",
          |  "taxCheckCodes": [ "ABC", "DEF" ],
          |  "languagePreference": "Welsh"
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
        true,
        Language.English,
        GGCredId("gg")
      )

      auditEvent.auditType       shouldBe "CompanyMatch"
      auditEvent.transactionName shouldBe "company-match"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "ctutrType": "Submitted",
          |  "matchResult": "NoMatch",
          |  "companyRegistrationNumber": "12345678",
          |  "submittedCTUTR": "1111111111",
          |  "submittedCTUTRStandardised": "2222222222",
          |  "hmrcCTUTR": "3333333333",
          |  "tooManyAttempts": true,
          |  "languagePreference": "English",
          |  "ggCredId": "gg"
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
        CTUTR("4444444444"),
        Language.Welsh,
        GGCredId("gg")
      )

      auditEvent.auditType       shouldBe "CompanyMatch"
      auditEvent.transactionName shouldBe "company-match"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "ctutrType": "Enrolment",
          |  "matchResult": "NoMatch",
          |  "companyRegistrationNumber": "12345678",
          |  "hmrcCTUTR": "3333333333",
          |  "enrolmentCTUTR": "4444444444",
          |  "languagePreference": "Welsh",
          |  "ggCredId": "gg"
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
        CTUTR("3333333333"),
        Language.English,
        GGCredId("gg")
      )

      auditEvent.auditType       shouldBe "CompanyMatch"
      auditEvent.transactionName shouldBe "company-match"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "ctutrType": "Submitted",
          |  "matchResult": "Match",
          |  "companyRegistrationNumber": "12345678",
          |  "submittedCTUTR": "1111111111",
          |  "submittedCTUTRStandardised": "2222222222",
          |  "hmrcCTUTR": "3333333333",
          |  "languagePreference": "English",
          |  "ggCredId": "gg"
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
        CTUTR("4444444444"),
        Language.Welsh,
        GGCredId("gg")
      )

      auditEvent.auditType       shouldBe "CompanyMatch"
      auditEvent.transactionName shouldBe "company-match"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "ctutrType": "Enrolment",
          |  "matchResult": "Match",
          |  "companyRegistrationNumber": "12345678",
          |  "hmrcCTUTR": "3333333333",
          |  "enrolmentCTUTR": "4444444444",
          |  "languagePreference": "Welsh",
          |  "ggCredId": "gg"
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
        None,
        None
      ),
      IndividualRetrievedJourneyData(None),
      IndividualUserAnswers.empty,
      None,
      Some(ZonedDateTime.of(2021, 12, 17, 16, 32, 33, 368000000, ZoneId.of("Europe/London"))),
      List.empty,
      hasConfirmedDetails = true,
      None,
      emailRequestedForTaxCheck = None,
      false,
      None,
      None,
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
        |        "dateOfBirth" : "2000-01-02"
        |    },
        |    "retrievedJourneyData" : {},
        |    "userAnswers" : {
        |        "type" : "Incomplete"
        |    },
        |    "taxCheckStartDateTime" : "2021-12-17T16:32:33.368Z[Europe/London]",
        |    "unexpiredTaxChecks" : [],
        |    "hasConfirmedDetails" : true,
        |    "hasResentEmailConfirmation" : false,
        |    "type" : "Individual"
        |}
        |""".stripMargin

    "SAUTRNotFound" in {
      test(
        TaxCheckExit.SAUTRNotFound(session, Language.English),
        Json.parse(
          s"""
            |{
            |  "serviceExitReason": "SAUTRNotFound",
            |  "serviceExitDescription": "SA UTR not found for the Applicant's NINO.",
            |  "source": "Digital",
            |  "taxCheckSessionData": $sessionJson,
            |  "languagePreference": "English"
            |}
            |""".stripMargin
        )
      )
    }

    "SANoNoticeToFileOrTaxReturn" in {
      test(
        TaxCheckExit.SANoNoticeToFileOrTaxReturn(session, Language.Welsh),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "SANoNoticeToFileOrTaxReturn",
             |  "serviceExitDescription": "For relevant income tax year, Self Assessment Notice to File not found, Self Assessment Tax Return not found.",
             |  "source": "Digital",
             |  "taxCheckSessionData": $sessionJson,
             |  "languagePreference": "Welsh"
             |}
             |""".stripMargin
        )
      )
    }

    "CTEnteredCTUTRNotMatchingBlocked" in {
      test(
        TaxCheckExit.CTEnteredCTUTRNotMatchingBlocked(session, Language.Welsh),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "CTEnteredCTUTRNotMatchingBlocked",
             |  "serviceExitDescription": "Applicant has made repeated attempts to provide a matching CT UTR. Attempts limit reached, so Applicant temporarily blocked from making an Application for that CRN.",
             |  "source": "Digital",
             |  "taxCheckSessionData": $sessionJson,
             |  "languagePreference": "Welsh"
             |}
             |""".stripMargin
        )
      )
    }

    "CTNoNoticeToFileOrTaxReturn" in {
      test(
        TaxCheckExit.CTNoNoticeToFileOrTaxReturn(session, Language.English),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "CTNoNoticeToFileOrTaxReturn",
             |  "serviceExitDescription": "For relevant accounting period, Corporation Tax Notice to File not found, Corporation Tax Return not found.",
             |  "source": "Digital",
             |  "taxCheckSessionData": $sessionJson,
             |  "languagePreference": "English"
             |}
             |""".stripMargin
        )
      )
    }

    "CTNoAccountingPeriodNotRecentlyStartedTrading" in {
      test(
        TaxCheckExit.CTNoAccountingPeriodNotRecentlyStartedTrading(session, Language.English),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "CTNoAccountingPeriodNotRecentlyStartedTrading",
             |  "serviceExitDescription": "No relevant accounting period was found on tax summary record for the lookback period, and the Applicant's Company has not recently started trading.",
             |  "source": "Digital",
             |  "taxCheckSessionData": $sessionJson,
             |  "languagePreference": "English"
             |}
             |""".stripMargin
        )
      )
    }

    "AllowedTaxChecksExceeded" in {
      test(
        TaxCheckExit.AllowedTaxChecksExceeded(session, Language.Welsh),
        Json.parse(
          s"""
             |{
             |  "serviceExitReason": "AllowedTaxChecksExceeded",
             |  "serviceExitDescription": "Attempted tax check for Licence Type exceeded number of permitted existing tax check codes, for a particular Applicant.",
             |  "source": "Digital",
             |  "taxCheckSessionData": $sessionJson,
             |  "languagePreference": "Welsh"
             |}
             |""".stripMargin
        )
      )
    }

  }

  "SubmitEmailAddressVerificationRequest" must {

    "have the correct JSON" when afterWord("the passcode request result is") {

      def test(auditEvent: SubmitEmailAddressVerificationRequest, expectedJson: JsValue) = {
        auditEvent.auditType       shouldBe "SubmitEmailAddressVerificationRequest"
        auditEvent.transactionName shouldBe "submit-email-address-verification-request"
        Json.toJson(auditEvent)    shouldBe expectedJson
      }

      "PasscodeSent" in {
        test(
          SubmitEmailAddressVerificationRequest(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.GGEmail,
            Some(PasscodeRequestResult.PasscodeSent),
            Language.Welsh
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "GovernmentGateway",
            |  "result": "Success",
            |  "languagePreference": "Welsh"
            |}
            |""".stripMargin
          )
        )
      }

      "EmailAddressAlreadyVerified" in {
        test(
          SubmitEmailAddressVerificationRequest(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Some(PasscodeRequestResult.EmailAddressAlreadyVerified),
            Language.English
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "result": "AlreadyVerified",
            |  "languagePreference": "English"
            |}
            |""".stripMargin
          )
        )
      }

      "BadEmailAddress" in {
        test(
          SubmitEmailAddressVerificationRequest(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Some(PasscodeRequestResult.BadEmailAddress),
            Language.Welsh
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "result": "Failure",
            |  "failureReason": "VerificationPasscodeEmailFailed",
            |  "languagePreference": "Welsh"
            |}
            |""".stripMargin
          )
        )
      }

      "MaximumNumberOfEmailsExceeded" in {
        test(
          SubmitEmailAddressVerificationRequest(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Some(PasscodeRequestResult.MaximumNumberOfEmailsExceeded),
            Language.English
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "result": "Failure",
            |  "failureReason": "AttemptsToVerifyEmailAddressExceeded",
            |  "languagePreference": "English"
            |}
            |""".stripMargin
          )
        )
      }

      "empty" in {
        test(
          SubmitEmailAddressVerificationRequest(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            None,
            Language.Welsh
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "result": "Failure",
            |  "failureReason": "TechnicalError",
            |  "languagePreference": "Welsh"
            |}
            |""".stripMargin
          )
        )
      }

    }

  }

  "SubmitEmailAddressVerificationPasscode" must {

    "have the correct JSON" when afterWord("the passcode verification result is") {

      def test(auditEvent: SubmitEmailAddressVerificationPasscode, expectedJson: JsValue) = {
        auditEvent.auditType       shouldBe "SubmitEmailAddressVerificationPasscode"
        auditEvent.transactionName shouldBe "submit-email-address-verification-passcode"
        Json.toJson(auditEvent)    shouldBe expectedJson
      }

      "Match" in {
        test(
          SubmitEmailAddressVerificationPasscode(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.GGEmail,
            Passcode("pass"),
            Some(PasscodeVerificationResult.Match),
            Language.English
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "GovernmentGateway",
            |  "verificationPasscode": "pass",
            |  "result": "Success",
            |  "languagePreference": "English"
            |}
            |""".stripMargin
          )
        )
      }

      "TooManyAttempts" in {
        test(
          SubmitEmailAddressVerificationPasscode(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Passcode("pass"),
            Some(PasscodeVerificationResult.TooManyAttempts),
            Language.English
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "verificationPasscode": "pass",
            |  "result": "Failure",
            |  "failureReason": "MaximumPasscodeVerificationAttemptsExceeded",
            |  "languagePreference": "English"
            |}
            |""".stripMargin
          )
        )
      }

      "Expired" in {
        test(
          SubmitEmailAddressVerificationPasscode(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Passcode("pass"),
            Some(PasscodeVerificationResult.Expired),
            Language.Welsh
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "verificationPasscode": "pass",
            |  "result": "Failure",
            |  "failureReason": "PasscodeNotFoundExpired",
            |  "languagePreference": "Welsh"
            |}
            |""".stripMargin
          )
        )
      }

      "NoMatch" in {
        test(
          SubmitEmailAddressVerificationPasscode(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Passcode("pass"),
            Some(PasscodeVerificationResult.NoMatch),
            Language.English
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "verificationPasscode": "pass",
            |  "result": "Failure",
            |  "failureReason": "PasscodeMismatch",
            |  "languagePreference": "English"
            |}
            |""".stripMargin
          )
        )
      }

      "empty" in {
        test(
          SubmitEmailAddressVerificationPasscode(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            Passcode("pass"),
            None,
            Language.Welsh
          ),
          Json.parse(
            """{
            |  "ggCredId": "credId",
            |  "taxCheckCode": "code",
            |  "emailAddress": "email",
            |  "emailSource": "Submitted",
            |  "verificationPasscode": "pass",
            |  "result": "Failure",
            |  "failureReason": "TechnicalError",
            |  "languagePreference": "Welsh"
            |}
            |""".stripMargin
          )
        )
      }

    }

  }

  "SendTaxCheckCodeNotificationEmail" must {

    "have the correct JSON" when afterWord("the passcode verification result is") {

      def test(auditEvent: SendTaxCheckCodeNotificationEmail, expectedJson: JsValue) = {
        auditEvent.auditType       shouldBe "SendTaxCheckCodeNotificationEmail"
        auditEvent.transactionName shouldBe "send-tax-check-code-notification-email"
        Json.toJson(auditEvent)    shouldBe expectedJson
      }

      "Sent" in {
        test(
          SendTaxCheckCodeNotificationEmail(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.GGEmail,
            "template",
            Some(EmailSendResult.EmailSent),
            Language.English
          ),
          Json.parse(
            """{
              |  "ggCredId": "credId",
              |  "taxCheckCode": "code",
              |  "emailAddress": "email",
              |  "emailSource": "GovernmentGateway",
              |  "emailTemplateIdentifier": "template",
              |  "result": "Success",
              |  "languagePreference": "English"
              |}
              |""".stripMargin
          )
        )
      }

      "NotSent" in {
        test(
          SendTaxCheckCodeNotificationEmail(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.DifferentEmail,
            "template",
            Some(EmailSendResult.EmailSentFailure),
            Language.Welsh
          ),
          Json.parse(
            """{
              |  "ggCredId": "credId",
              |  "taxCheckCode": "code",
              |  "emailAddress": "email",
              |  "emailSource": "Submitted",
              |  "emailTemplateIdentifier": "template",
              |  "result": "Failure",
              |  "languagePreference": "Welsh"
              |}
              |""".stripMargin
          )
        )
      }

      "empty" in {
        test(
          SendTaxCheckCodeNotificationEmail(
            GGCredId("credId"),
            HECTaxCheckCode("code"),
            EmailAddress("email"),
            EmailType.GGEmail,
            "template",
            None,
            Language.English
          ),
          Json.parse(
            """{
              |  "ggCredId": "credId",
              |  "taxCheckCode": "code",
              |  "emailAddress": "email",
              |  "emailSource": "GovernmentGateway",
              |  "emailTemplateIdentifier": "template",
              |  "result": "Failure",
              |  "languagePreference": "English"
              |}
              |""".stripMargin
          )
        )
      }

    }

  }

  "ApplicantServiceStartEndPointAccessed" must {

    "have the correct JSON" in {
      val auditEvent = ApplicantServiceStartEndPointAccessed(
        AuthenticationStatus.Authenticated,
        Some("url"),
        Some(
          AuthenticationDetails(
            "provider",
            "id",
            Some(AffinityGroup.Organisation),
            Some(EntityType.Individual),
            ConfidenceLevel.L250
          )
        )
      )

      auditEvent.auditType       shouldBe "ApplicantServiceStartEndPointAccessed"
      auditEvent.transactionName shouldBe "applicant-service-start-end-point-accessed"
      Json.toJson(auditEvent)    shouldBe Json.parse(
        """
          |{
          |  "authenticationStatus": "Authenticated",
          |  "redirectionUrl": "url",
          |  "authenticationDetail": {
          |    "authenticationProvider": "provider",
          |    "authenticationProviderCredId": "id",
          |    "ggAffinityGroup": "Organisation",
          |    "entityType": "Individual",
          |    "confidenceLevel": "CL250"
          |  }
          |}
          |""".stripMargin
      )
    }

  }

}
