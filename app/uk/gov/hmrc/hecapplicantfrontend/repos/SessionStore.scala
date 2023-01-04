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

package uk.gov.hmrc.hecapplicantfrontend.repos

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Request
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession}
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.mongo.cache.{DataKey, SessionCacheRepository}
import uk.gov.hmrc.mongo.{CurrentTimestampSupport, MongoComponent}
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SessionStoreImpl])
trait SessionStore {

  def get()(implicit request: Request[_]): EitherT[Future, Error, Option[HECSession]]

  def store(sessionData: HECSession)(implicit
    request: Request[_]
  ): EitherT[Future, Error, Unit]

  def delete()(implicit request: Request[_]): EitherT[Future, Error, Unit]

}

@Singleton
class SessionStoreImpl @Inject() (
  mongo: MongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends SessionCacheRepository(
      mongoComponent = mongo,
      collectionName = "sessions",
      ttl = configuration.get[FiniteDuration]("session-store.expiry-time"),
      timestampSupport = new CurrentTimestampSupport(),
      sessionIdKey = SessionKeys.sessionId
    )
    with SessionStore {

  val sessionKey: DataKey[HECSession] = DataKey("hec-session")

  def get()(implicit request: Request[_]): EitherT[Future, Error, Option[HECSession]] =
    EitherT(
      preservingMdc {
        getFromSession[HECSession](sessionKey)
          .map(Right(_))
          .recover { case e ⇒ Left(Error(e)) }
      }
    )

  def store(
    sessionData: HECSession
  )(implicit request: Request[_]): EitherT[Future, Error, Unit] =
    EitherT(preservingMdc {
      putSession[HECSession](sessionKey, sessionData)
        .map(_ => Right(()))
        .recover { case e => Left(Error(e)) }
    })

  def delete()(implicit request: Request[_]): EitherT[Future, Error, Unit] =
    EitherT(preservingMdc {
      deleteFromSession(sessionKey)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    })

}
