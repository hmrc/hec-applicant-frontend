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
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.http.Status.ACCEPTED
import uk.gov.hmrc.hecapplicantfrontend.connectors.{HECConnector, SendEmailConnector}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.SendTaxCheckCodeNotificationEmail
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, Language, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.{EmailParameters, EmailSendRequest, EmailSendResult}
import uk.gov.hmrc.hecapplicantfrontend.models.Language.{English, Welsh}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.SaveEmailAddressRequest
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SendEmailServiceImpl])
trait SendEmailService {

  def sendEmail(userSelectedEmail: UserSelectedEmail, emailParameters: EmailParameters)(implicit
    hc: HeaderCarrier,
    r: RequestWithSessionData[_]
  ): EitherT[Future, Error, EmailSendResult]

}

@Singleton
class SendEmailServiceImpl @Inject() (
  emailSendConnector: SendEmailConnector,
  hecConnector: HECConnector,
  auditService: AuditService,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends SendEmailService
    with Logging {

  val templateIdEN: String = configuration.get[String]("email-send.template-id-en")
  val templateIdCY: String = configuration.get[String]("email-send.template-id-cy")

  override def sendEmail(userSelectedEmail: UserSelectedEmail, emailParameters: EmailParameters)(implicit
    hc: HeaderCarrier,
    r: RequestWithSessionData[_]
  ): EitherT[Future, Error, EmailSendResult] = {
    val taxCheckCode =
      r.sessionData.emailRequestedForTaxCheck
        .map(_.taxCheck.taxCheckCode)
        .getOrElse(sys.error("Could not find tax check code"))

    def auditEvent(templateId: String, result: Option[EmailSendResult]): SendTaxCheckCodeNotificationEmail =
      SendTaxCheckCodeNotificationEmail(
        r.sessionData.loginData.ggCredId,
        taxCheckCode,
        userSelectedEmail.emailAddress,
        userSelectedEmail.emailType,
        templateId,
        result
      )

    EitherT
      .fromEither[Future](Language.fromRequest(r.messagesRequest))
      .leftMap((Error(_)))
      .flatMap { lang =>
        val templateId                                   = getTemplateId(lang)
        val result: EitherT[Future, Error, HttpResponse] = emailSendConnector.sendEmail(
          EmailSendRequest(List(userSelectedEmail.emailAddress), templateId, emailParameters)
        )

        result
          .leftMap { e =>
            auditService.sendEvent(auditEvent(templateId, None))
            e
          }
          .subflatMap { response =>
            val result = response.status match {
              case ACCEPTED =>
                val _ =
                  hecConnector.saveEmailAddress(SaveEmailAddressRequest(userSelectedEmail.emailAddress, taxCheckCode))
                Right(EmailSendResult.EmailSent)
              case other    =>
                logger.warn(s"Response for send email call came back with status : $other")
                Right(EmailSendResult.EmailSentFailure)
            }

            auditService.sendEvent(auditEvent(templateId, result.toOption))
            result
          }
      }
  }

  private def getTemplateId(lang: Language) = lang match {
    case English => templateIdEN
    case Welsh   => templateIdCY
  }
}
