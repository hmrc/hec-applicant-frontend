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

package uk.gov.hmrc.hecapplicantfrontend.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendRequest
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.client.HttpClientV2
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.ws.writeableOf_JsValue
import uk.gov.hmrc.http.HttpReads.Implicits.*

@ImplementedBy(classOf[SendEmailConnectorImpl])
trait SendEmailConnector {
  def sendEmail(emailSendRequest: EmailSendRequest)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]
}

@Singleton
class SendEmailConnectorImpl @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends SendEmailConnector {

  private val baseUrl: String = servicesConfig.baseUrl("email-send")

  override def sendEmail(emailSendRequest: EmailSendRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] = EitherT[Future, Error, HttpResponse](
    http
      .post(url"$baseUrl/hmrc/email")
      .withBody(Json.toJson(emailSendRequest))
      .execute[HttpResponse]
      .map(Right(_))
      .recover { case e => Left(Error(e)) }
  )
}
