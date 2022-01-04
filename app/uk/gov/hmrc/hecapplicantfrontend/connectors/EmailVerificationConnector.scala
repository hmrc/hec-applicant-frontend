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

package uk.gov.hmrc.hecapplicantfrontend.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{PasscodeRequest, PasscodeVerificationRequest}
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[EmailVerificationConnectorImpl])
trait EmailVerificationConnector {

  def requestPasscode(passcodeRequest: PasscodeRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def verifyPasscode(passcodeVerificationRequest: PasscodeVerificationRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class EmailVerificationConnectorImpl @Inject() (
  http: HttpClient,
  servicesConfig: ServicesConfig
)(implicit
  ec: ExecutionContext
) extends EmailVerificationConnector {

  val baseUrl: String = servicesConfig.baseUrl("email-verification")

  val requestPasscodeUrl: String = s"$baseUrl/email-verification/request-passcode"

  val verifyPasscodeUrl: String = s"$baseUrl/email-verification/verify-passcode"

  override def requestPasscode(passcodeRequest: PasscodeRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .POST[PasscodeRequest, HttpResponse](requestPasscodeUrl, passcodeRequest)
        .map(Right(_))
        .recover { case e ⇒
          Left(Error(e))
        }
    )
  override def verifyPasscode(passcodeVerificationRequest: PasscodeVerificationRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .POST[PasscodeVerificationRequest, HttpResponse](verifyPasscodeUrl, passcodeVerificationRequest)
        .map(Right(_))
        .recover { case e ⇒
          Left(Error(e))
        }
    )

}
