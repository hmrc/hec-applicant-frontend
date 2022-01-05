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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.libs.json.Json
import play.api.mvc.{AnyContentAsEmpty, Cookie, MessagesRequest}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.connectors.SendEmailConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.ControllerSpec
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.{EmailParameters, EmailSendRequest, EmailSendResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Language.{English, Welsh}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class SendEmailServiceImplSpec extends AnyWordSpec with Matchers with MockFactory with ControllerSpec {

  val mockSendEmailConnector: SendEmailConnector = mock[SendEmailConnector]
  val config                                     = Configuration(
    ConfigFactory.parseString(s"""
                                 | email-send {
                                 |    template-id-en = "template_EN"
                                 |    template-id-cy =  "template_CY"
                                 |  }
                                 |""".stripMargin)
  )

  override def additionalConfig = super.additionalConfig.withFallback(
    Configuration(
      ConfigFactory.parseString(
        s"""
           | play.i18n.langs = ["en", "cy", "fr"]
           |""".stripMargin
      )
    )
  )

  def mockSendEmail(emailSendRequest: EmailSendRequest)(result: Either[Error, HttpResponse]) =
    (mockSendEmailConnector
      .sendEmail(_: EmailSendRequest)(_: HeaderCarrier))
      .expects(emailSendRequest, *)
      .returning(EitherT.fromEither[Future](result))

  val sendEmailService           = new SendEmailServiceImpl(mockSendEmailConnector, config)
  implicit val hc: HeaderCarrier = HeaderCarrier()
  val emailAddress               = EmailAddress("user@test.com")
  val emailParameter             =
    EmailParameters("Dummy name", "ABC 123 GRD")
  val emptyHeaders               = Map.empty[String, Seq[String]]
  val ggEmailId                  = EmailAddress("user@test.com")

  val userEmailAnswer = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
      passcode = Passcode("HHHHHH").some,
      passcodeVerificationResult = PasscodeVerificationResult.Match.some
    )

  val session: HECSession = Fixtures.companyHECSession(
    loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
    userAnswers = Fixtures.completeCompanyUserAnswers(),
    isEmailRequested = true,
    userEmailAnswers = userEmailAnswer.some
  )

  "SendEmailServiceImplSpec" when {

    " handling request to send email" must {

      "return an error" when {

        val emailSendRequest                                                                = EmailSendRequest(List(emailAddress), "template_EN", emailParameter)
        val authenticatedRequest                                                            = AuthenticatedRequest(
          new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "en")), messagesApi)
        )
        implicit val requestWithSessionData: RequestWithSessionData[AnyContentAsEmpty.type] =
          RequestWithSessionData(authenticatedRequest, session)

        val emailSendRequestJson = Json.toJson(emailSendRequest)

        def testError() = {
          val result = sendEmailService.sendEmail(emailAddress, emailParameter)
          await(result.value) shouldBe a[Left[_, _]]
        }

        "the http call fails" in {
          mockSendEmail(emailSendRequest)(Left(Error("")))
          testError()
        }

        "the http response does not come back with status 202 (Accepted)" in {
          mockSendEmail(emailSendRequest)(Right(HttpResponse(OK, emailSendRequestJson, emptyHeaders)))
          testError()
        }

        "there is no json in the response" in {
          mockSendEmail(emailSendRequest)(Right(HttpResponse(CREATED, "hi")))
          testError()
        }

        "the json in the response cannot be parsed" in {
          val json = Json.parse("""{ "a" : 1 }""")
          mockSendEmail(emailSendRequest)(Right(HttpResponse(CREATED, json, emptyHeaders)))
          testError()
        }

        "Language in the session is not either en or cy" in {
          val authenticatedRequest: AuthenticatedRequest[AnyContentAsEmpty.type]    = AuthenticatedRequest(
            new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", "fr")), messagesApi)
          )
          implicit val requestWiths: RequestWithSessionData[AnyContentAsEmpty.type] =
            RequestWithSessionData(authenticatedRequest, session)
          val result                                                                = sendEmailService.sendEmail(emailAddress, emailParameter)(hc, requestWiths)
          await(result.value) shouldBe a[Left[_, _]]
        }
      }

      "return successfully" when {
        def authenticatedRequest(lang: String) = AuthenticatedRequest(
          new MessagesRequest(FakeRequest().withCookies(Cookie("PLAY_LANG", lang)), messagesApi)
        )

        def getEmailSendRequest(templateId: String) = EmailSendRequest(List(emailAddress), templateId, emailParameter)

        "request json can be parsed and email is send " in {

          Map(English.code -> "template_EN", Welsh.code -> "template_CY").foreach { keyValue =>
            withClue(s"For lang: ${keyValue._1} and templateId: ${keyValue._2}") {
              val lang                                                                            = keyValue._1
              val templateId                                                                      = keyValue._2
              val emailSendRequest                                                                = getEmailSendRequest(templateId)
              val request                                                                         = authenticatedRequest(lang)
              implicit val requestWithSessionData: RequestWithSessionData[AnyContentAsEmpty.type] =
                RequestWithSessionData(request, session)
              mockSendEmail(emailSendRequest)(Right(HttpResponse(ACCEPTED, "")))

              val result = sendEmailService.sendEmail(emailAddress, emailParameter)
              await(result.value) shouldBe Right(EmailSendResult.EmailSent)

            }
          }

        }

      }

    }
  }
}
