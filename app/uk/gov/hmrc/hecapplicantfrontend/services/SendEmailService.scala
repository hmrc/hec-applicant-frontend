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
import cats.implicits.catsSyntaxEq
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.http.Status.ACCEPTED
import uk.gov.hmrc.hecapplicantfrontend.connectors.SendEmailConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthenticatedRequest
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error}
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.{EmailParameters, EmailSendRequest, EmailSendResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Language
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Language.English
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SendEmailServiceImpl])
trait SendEmailService {
  def sendEmail(emailAddress: EmailAddress, emailParameters: EmailParameters)(implicit
    hc: HeaderCarrier,
    r: AuthenticatedRequest[_]
  ): EitherT[Future, Error, EmailSendResult]

}

@Singleton
class SendEmailServiceImpl @Inject() (emailSendConnector: SendEmailConnector, configuration: Configuration)(implicit
  ec: ExecutionContext
) extends SendEmailService {
  override def sendEmail(emailAddress: EmailAddress, emailParameters: EmailParameters)(implicit
    hc: HeaderCarrier,
    r: AuthenticatedRequest[_]
  ): EitherT[Future, Error, EmailSendResult] = {
    val result: EitherT[Future, Error, HttpResponse] = for {
      lang      <- EitherT.fromEither[Future](Language.fromRequest(r)).leftMap(Error(_))
      templateId = if (lang.code === English.code) configuration.get[String]("email-send.template-id-en")
                   else configuration.get[String]("email-send.template-id-cy")
      result    <- emailSendConnector.sendEmail(EmailSendRequest(List(emailAddress), templateId, emailParameters))
    } yield result

    result.subflatMap { response =>
      response.status match {
        case ACCEPTED => Right(EmailSendResult.EmailSent)
        case other    =>
          Left(
            Error(s"Call to sendEmail came back with status $other")
          )
      }

    }

  }
}
