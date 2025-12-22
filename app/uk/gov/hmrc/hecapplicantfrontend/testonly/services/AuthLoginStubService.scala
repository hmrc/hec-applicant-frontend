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

package uk.gov.hmrc.hecapplicantfrontend.testonly.services

import cats.data.EitherT
import cats.syntax.either.*
import cats.syntax.eq.*
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.*
import play.api.mvc.{Cookie, Session, SessionCookieBaker}
import uk.gov.hmrc.crypto.Crypted
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.testonly.connectors.AuthLoginStubConnector
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.LoginData
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[AuthLoginStubServiceImpl])
trait AuthLoginStubService {

  def login(loginData: LoginData)(implicit hc: HeaderCarrier): EitherT[Future, Error, Session]

}

@Singleton
class AuthLoginStubServiceImpl @Inject() (
  connector: AuthLoginStubConnector,
  sessionCookieCrypto: SessionCookieCrypto,
  sessionCookieBaker: SessionCookieBaker
)(implicit
  ec: ExecutionContext
) extends AuthLoginStubService {

  override def login(
    loginData: LoginData
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Session] =
    connector.login(loginData).subflatMap { httpResponse =>
      val status        = httpResponse.status
      lazy val location = httpResponse.header("location")
      if (status =!= SEE_OTHER)
        Left(
          Error(
            s"Call to login came back with status $status. Body was ${httpResponse.body}"
          )
        )
      else if (!location.contains(loginData.redirectUrl))
        Left(Error(s"Location header value [${location.getOrElse("")}] of response was not ${loginData.redirectUrl}"))
      else {
        val sessionOpt =
          for {
            cookie   <- httpResponse.cookie("mdtp")
            decrypted = Cookie(name = "mdtp", value = sessionCookieCrypto.crypto.decrypt(Crypted(cookie.value)).value)
            session   = sessionCookieBaker.decodeFromCookie(Some(decrypted))
          } yield session

        Either.fromOption(sessionOpt, Error("Could not extract session"))
      }
    }

}
