/*
 * Copyright 2021 HM Revenue & Customs
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

package uk.gov.hmrc.hecapplicantfrontend.testonly.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import configs.syntax._
import play.api.Configuration
import play.api.libs.ws.{WSClient, WSResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.LoginData
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.http.{HeaderCarrier}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[AuthLoginStubConnectorImpl])
trait AuthLoginStubConnector {

  def login(loginData: LoginData)(implicit hc: HeaderCarrier): EitherT[Future, Error, WSResponse]

}

@Singleton
class AuthLoginStubConnectorImpl @Inject() (config: Configuration, ws: WSClient)(implicit
  ec: ExecutionContext
) extends AuthLoginStubConnector {

  val authLoginStubUrl: String = {
    val baseUrl = config.underlying.get[String]("auth-login-stub.base-url").value
    s"$baseUrl/auth-login-stub/gg-sign-in"
  }

  def requestFormBody(loginData: LoginData): Map[String, String] = {
    val enrolmentIdentifier = loginData.enrolment.flatMap(_.identifiers.headOption)

    List(
      "authorityId"                         -> Some(loginData.ggCredId.value),
      "redirectionUrl"                      -> Some(loginData.redirectUrl),
      "credentialStrength"                  -> Some("strong"),
      "confidenceLevel"                     -> Some(loginData.confidenceLevel.level.toString),
      "affinityGroup"                       -> Some(loginData.affinityGroup.toString),
      "credentialRole"                      -> Some("User"),
      "email"                               -> Some(loginData.email.value),
      "nino"                                -> loginData.nino.map(_.value),
      "enrolment[0].name"                   -> loginData.enrolment.map(_.key),
      "enrolment[0].taxIdentifier[0].name"  -> enrolmentIdentifier.map(_.key),
      "enrolment[0].taxIdentifier[0].value" -> enrolmentIdentifier.map(_.value),
      "enrolment[0].state"                  -> loginData.enrolment.map(_.state)
    ).collect { case (k, Some(v)) => k -> v }.toMap
  }

  override def login(loginData: LoginData)(implicit hc: HeaderCarrier): EitherT[Future, Error, WSResponse] = {
    val formData =
      requestFormBody(loginData).map { case (k, v) => s"${k.urlEncode}=${v.urlEncode}" }.mkString("&")
    EitherT[Future, Error, WSResponse](
      ws.url(authLoginStubUrl)
        .withFollowRedirects(false)
        .withHttpHeaders("Content-Type" -> "application/x-www-form-urlencoded")
        .post(formData)
        .map((r: WSResponse) => Right(r))
        .recover { case NonFatal(e) => Left(Error(e)) }
    )
  }

}
