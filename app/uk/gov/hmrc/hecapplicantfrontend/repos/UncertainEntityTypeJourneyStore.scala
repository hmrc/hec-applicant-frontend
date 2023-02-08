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
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, UncertainEntityTypeJourney}
import uk.gov.hmrc.http.SessionKeys
import uk.gov.hmrc.mongo.{CurrentTimestampSupport, MongoComponent}
import uk.gov.hmrc.mongo.cache.{DataKey, SessionCacheRepository}
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration.FiniteDuration

@ImplementedBy(classOf[UncertainEntityTypeJourneyStoreImpl])
trait UncertainEntityTypeJourneyStore {

  def get()(implicit request: Request[_]): EitherT[Future, Error, Option[UncertainEntityTypeJourney]]

  def store(journey: UncertainEntityTypeJourney)(implicit
    request: Request[_]
  ): EitherT[Future, Error, Unit]

}

@Singleton
class UncertainEntityTypeJourneyStoreImpl @Inject() (
  mongo: MongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends SessionCacheRepository(
      mongoComponent = mongo,
      collectionName = "uncertain-entity-type-journeys",
      ttl = configuration.get[FiniteDuration]("session-store.expiry-time"),
      timestampSupport = new CurrentTimestampSupport(),
      sessionIdKey = SessionKeys.sessionId
    )
    with UncertainEntityTypeJourneyStore {

  val sessionKey: String = "session"

  def get()(implicit request: Request[_]): EitherT[Future, Error, Option[UncertainEntityTypeJourney]] =
    EitherT(
      preservingMdc {
        getFromSession[UncertainEntityTypeJourney](DataKey(sessionKey))
          .map(Right(_))
          .recover { case e => Left(Error(e)) }
      }
    )

  def store(
    journey: UncertainEntityTypeJourney
  )(implicit request: Request[_]): EitherT[Future, Error, Unit] =
    EitherT(preservingMdc {
      putSession[UncertainEntityTypeJourney](DataKey(sessionKey), journey)
        .map(_ => Right(()))
        .recover { case e => Left(Error(e)) }
    })

}
