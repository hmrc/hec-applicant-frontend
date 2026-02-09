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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.i18n.MessagesApi
import play.api.libs.json.Json
import play.api.mvc.MessagesRequest
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.hecapplicantfrontend.connectors.{HECConnector, SendEmailConnector}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.SendTaxCheckCodeNotificationEmail
import uk.gov.hmrc.hecapplicantfrontend.models.Language
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailRequestedForTaxCheck, EmailType, Error, HECSession, HECTaxCheckCode, TaxCheckListItem, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.{EmailParameters, EmailSendRequest, EmailSendResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.SaveEmailAddressRequest
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SendEmailServiceImplSpec extends AnyWordSpec with Matchers with MockFactory with AuditServiceSupport {

  val mockSendEmailConnector: SendEmailConnector = mock[SendEmailConnector]
  val mockHecConnector: HECConnector             = mock[HECConnector]

  val config = Configuration(
    ConfigFactory.parseString(s"""
                                 | email-send {
                                 |    template-id-en = "template_EN"
                                 |    template-id-cy =  "template_CY"
                                 |  }
                                 |""".stripMargin)
  )

  def mockSendEmail(emailSendRequest: EmailSendRequest)(result: Either[Error, HttpResponse]) =
    (mockSendEmailConnector
      .sendEmail(_: EmailSendRequest)(_: HeaderCarrier))
      .expects(emailSendRequest, *)
      .returning(EitherT.fromEither[Future](result))

  def mockSaveEmailAddress(saveEmailAddressRequest: SaveEmailAddressRequest)(result: Either[Error, HttpResponse]) =
    (mockHecConnector
      .saveEmailAddress(_: SaveEmailAddressRequest)(_: HeaderCarrier))
      .expects(saveEmailAddressRequest, *)
      .returning(EitherT.fromEither[Future](result))

  val sendEmailService           = new SendEmailServiceImpl(mockSendEmailConnector, mockHecConnector, mockAuditService, config)
  implicit val hc: HeaderCarrier = HeaderCarrier()
  val userSelectedEmail          = UserSelectedEmail(EmailType.GGEmail, EmailAddress("user@test.com"))
  val emailParameter             =
    EmailParameters("9 July 2021", "Driver of taxis and private hires", "ABC 123 GRD", "9 October 2021")

  val emailParametersCY = EmailParameters(
    "9 Gorffennaf 2021",
    "Gyrrwr tacsis a hurio preifat",
    "ABC 123 DER",
    "9 Hydref 2021"
  )
  val emptyHeaders      = Map.empty[String, Seq[String]]

  val userEmailAnswer = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
      passcode = Passcode("HHHHHH").some,
      passcodeVerificationResult = PasscodeVerificationResult.Match.some
    )

  val taxCheckCode = HECTaxCheckCode("code")

  val session: HECSession = Fixtures.companyHECSession(
    loginData = Fixtures.companyLoginData(emailAddress = userSelectedEmail.emailAddress.some),
    userAnswers = Fixtures.completeCompanyUserAnswers(),
    emailRequestedForTaxCheck = Some(
      EmailRequestedForTaxCheck(
        "",
        TaxCheckListItem(LicenceType.ScrapMetalMobileCollector, taxCheckCode, LocalDate.now(), ZonedDateTime.now())
      )
    ),
    userEmailAnswers = userEmailAnswer.some
  )

  def auditEvent(templateId: String, result: Option[EmailSendResult], language: Language) =
    SendTaxCheckCodeNotificationEmail(
      session.loginData.ggCredId,
      taxCheckCode,
      userSelectedEmail.emailAddress,
      userSelectedEmail.emailType,
      templateId,
      result,
      language
    )

  val authenticatedRequest =
    AuthenticatedRequest(
      new MessagesRequest(FakeRequest(), stub[MessagesApi])
    )

  "SendEmailServiceImplSpec" when {

    "handling request to send email" must {

      "return an error" when {

        val emailSendRequest =
          EmailSendRequest(List(userSelectedEmail.emailAddress), "template_EN", emailParameter)

        implicit val requestWithSessionData: RequestWithSessionData[?] =
          RequestWithSessionData(authenticatedRequest, session, Language.English)

        def testError() = {
          val result = sendEmailService.sendEmail(userSelectedEmail, emailParameter)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http call fails" in {
          inSequence {
            mockSendEmail(emailSendRequest)(Left(Error("")))
            mockSendAuditEvent(auditEvent("template_EN", None, Language.English))
          }

          testError()
        }

      }

      "return successfully" when {
        def getEmailSendRequest(templateId: String) =
          if (templateId === "template_EN")
            EmailSendRequest(List(userSelectedEmail.emailAddress), templateId, emailParameter)
          else
            EmailSendRequest(List(userSelectedEmail.emailAddress), templateId, emailParametersCY)

        def test(saveEmailAddressResponse: () => Either[Error, HttpResponse]) =
          Map(
            Language.English -> "template_EN",
            Language.Welsh   -> "template_CY"
          ).foreach { case (lang, templateId) =>
            withClue(s"For lang: $lang and templateId: $templateId") {
              val emailSendRequest                                           = getEmailSendRequest(templateId)
              implicit val requestWithSessionData: RequestWithSessionData[?] =
                RequestWithSessionData(authenticatedRequest, session, lang)

              inSequence {
                mockSendEmail(emailSendRequest)(Right(HttpResponse(ACCEPTED, "")))
                mockSaveEmailAddress(
                  SaveEmailAddressRequest(
                    userSelectedEmail.emailAddress,
                    taxCheckCode
                  )
                )(saveEmailAddressResponse())
                mockSendAuditEvent(auditEvent(templateId, Some(EmailSendResult.EmailSent), lang))
              }

              val result =
                if (templateId === "template_EN")
                  sendEmailService.sendEmail(userSelectedEmail, emailParameter)
                else
                  sendEmailService.sendEmail(userSelectedEmail, emailParametersCY)
              await(result.value) shouldBe Right(EmailSendResult.EmailSent)

            }
          }

        "request json can be parsed and email is sent" in {
          test(() => Right(HttpResponse(200, "")))
        }

        "request json can be parsed and email is sent but the call to save an email address fails" in {
          test(() => Left(Error("")))
        }

        "request json can be parsed and email is sent but the call to save an email address comes back with a non-200 response" in {
          test(() => Right(HttpResponse(404, "")))
        }

      }

      "return an EmailSentFailure" when {

        "Email Service fails to send email " when {

          def testIsEmailSendFailure(httpResponse: HttpResponse) = {
            val emailSendRequest                                           =
              EmailSendRequest(List(userSelectedEmail.emailAddress), "template_EN", emailParameter)
            implicit val requestWithSessionData: RequestWithSessionData[?] =
              RequestWithSessionData(authenticatedRequest, session, Language.English)
            val expectedAuditEvent                                         = auditEvent("template_EN", Some(EmailSendResult.EmailSentFailure), Language.English)

            inSequence {
              mockSendEmail(emailSendRequest)(Right(httpResponse))
              mockSendAuditEvent(expectedAuditEvent)
            }

            val result = sendEmailService.sendEmail(userSelectedEmail, emailParameter)
            await(result.value) shouldBe Right(EmailSendResult.EmailSentFailure)
          }

          "the http response does not come back with status 202 (Accepted)" in {
            testIsEmailSendFailure(HttpResponse(OK, "", emptyHeaders))
          }

          "there is no json in the response" in {
            testIsEmailSendFailure(HttpResponse(CREATED, "hi"))
          }

          "the json in the response cannot be parsed" in {
            val json = Json.parse("""{ "a" : 1 }""")
            testIsEmailSendFailure(HttpResponse(BAD_REQUEST, json, emptyHeaders))
          }
        }
      }

    }
  }
}
