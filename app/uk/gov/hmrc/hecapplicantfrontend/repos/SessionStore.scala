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

package uk.gov.hmrc.hecapplicantfrontend.repos

import configs.syntax._
import cats.data.{EitherT, OptionT}
import cats.instances.either._
import cats.instances.future._
import cats.syntax.either._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import play.api.libs.json.{Json, Reads, Writes}
import play.modules.reactivemongo.ReactiveMongoComponent
import uk.gov.hmrc.cache.model.Id
import uk.gov.hmrc.cache.repository.CacheMongoRepository
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SessionStoreImpl])
trait SessionStore {

  def get()(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[HECSession]]

  def store(sessionData: HECSession)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit]

}

@Singleton
class SessionStoreImpl @Inject() (
  mongo: ReactiveMongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends SessionStore {

  val cacheRepository: CacheMongoRepository = {
    val expireAfter: FiniteDuration = configuration.underlying
      .get[FiniteDuration]("session-store.expiry-time")
      .value

    new CacheMongoRepository("sessions", expireAfter.toSeconds)(
      mongo.mongoConnector.db,
      ec
    )
  }

  val sessionKey: String = "hec-session"

  def get()(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[HECSession]] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) ⇒
        EitherT(doGet[HECSession](sessionId))
      case None =>
        EitherT.leftT(Error("no session id found in headers - cannot query mongo"))
    }

  def store(
    sessionData: HECSession
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    hc.sessionId.map(_.value) match {
      case Some(sessionId) ⇒ EitherT(doStore(sessionId, sessionData))
      case None ⇒
        EitherT.leftT(
          Error("no session id found in headers - cannot store data in mongo")
        )
    }

  private def doGet[A : Reads](
    id: String
  )(implicit ec: ExecutionContext): Future[Either[Error, Option[A]]] =
    preservingMdc {
      cacheRepository
        .findById(Id(id))
        .map { maybeCache =>
          val response: OptionT[Either[Error, *], A] = for {
            cache ← OptionT.fromOption[Either[Error, *]](maybeCache)
            data ← OptionT.fromOption[Either[Error, *]](cache.data)
            result ← OptionT.liftF[Either[Error, *], A](
                       (data \ sessionKey)
                         .validate[A]
                         .asEither
                         .leftMap(e ⇒
                           Error(
                             s"Could not parse session data from mongo: ${e.mkString("; ")}"
                           )
                         )
                     )
          } yield result

          response.value
        }
        .recover { case e ⇒ Left(Error(e)) }
    }

  private def doStore[A : Writes](id: String, a: A)(implicit
    ec: ExecutionContext
  ): Future[Either[Error, Unit]] =
    preservingMdc {
      cacheRepository
        .createOrUpdate(Id(id), sessionKey, Json.toJson(a))
        .map[Either[Error, Unit]] { dbUpdate ⇒
          if (dbUpdate.writeResult.inError)
            Left(
              Error(
                dbUpdate.writeResult.errmsg.getOrElse(
                  "unknown error during inserting session data in mongo"
                )
              )
            )
          else
            Right(())
        }
        .recover { case e ⇒ Left(Error(e)) }
    }

}
