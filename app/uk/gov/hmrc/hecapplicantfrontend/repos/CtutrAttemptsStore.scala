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

import cats.data.{EitherT, OptionT}
import cats.implicits.toBifunctorOps
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.{CtutrAttempts, Error}
import uk.gov.hmrc.mongo.cache.{CacheIdType, DataKey, MongoCacheRepository}
import uk.gov.hmrc.mongo.{CurrentTimestampSupport, MongoComponent}
import uk.gov.hmrc.play.http.logging.Mdc.preservingMdc

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CtutrAttemptsStoreImpl])
trait CtutrAttemptsStore {

  def get(crn: CRN, ggCredId: GGCredId): EitherT[Future, Error, Option[CtutrAttempts]]

  def store(ctutrAttempts: CtutrAttempts): EitherT[Future, Error, Unit]

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, Error, Unit]

}

@Singleton
class CtutrAttemptsStoreImpl @Inject() (
  mongo: MongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends MongoCacheRepository(
      mongoComponent = mongo,
      collectionName = "ctutrAttempts",
      ttl = configuration.get[FiniteDuration]("ctutr-attempts-store-expiry-time"),
      timestampSupport = new CurrentTimestampSupport(),
      cacheIdType = CacheIdType.SimpleCacheId // Here, CacheId to be represented with `String`
    )
    with CtutrAttemptsStore {

  val dataKey: String = "hec-ctutr-attempts"

  private def id(crn: CRN, ggCredId: GGCredId) = s"${crn.value}-${ggCredId.value}"

  def get(crn: CRN, ggCredId: GGCredId): EitherT[Future, Error, Option[CtutrAttempts]] =
    EitherT(
      preservingMdc {
        findById(id(crn, ggCredId))
          .map { maybeCache =>
            val response: OptionT[Either[Error, *], CtutrAttempts] = for {
              cache ← OptionT.fromOption[Either[Error, *]](maybeCache)
              // TODO rewrite
              // even if there is no data , cache returns with -> {"id" : "code1", data : {}}
              //so added a logic if the json is empty, then return None
              // but if there is, then proceed to validate json
              cacheLength = cache.data.keys.size
              data       <- OptionT.fromOption[Either[Error, *]](if (cacheLength == 0) None else Some(cache.data))
              result ← OptionT.liftF[Either[Error, *], CtutrAttempts](
                         (data \ dataKey)
                           .validate[CtutrAttempts]
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
    )

  def store(
    ctutrAttempts: CtutrAttempts
  ): EitherT[Future, Error, Unit] =
    EitherT(preservingMdc {
      put[CtutrAttempts](id(ctutrAttempts.crn, ctutrAttempts.ggCredId))(DataKey(dataKey), ctutrAttempts)
        .map(_ => Right(()))
        .recover { case e => Left(Error(e)) }
    })

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, Error, Unit] =
    EitherT(preservingMdc {
      deleteEntity(id(crn, ggCredId))
        .map(_ => Right(()))
        .recover { case e => Left(Error(e)) }
    })
}
