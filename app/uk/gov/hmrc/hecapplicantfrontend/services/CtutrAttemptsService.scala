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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.hecapplicantfrontend.models
import uk.gov.hmrc.hecapplicantfrontend.models.CtutrAttempts
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.repos.CtutrAttemptsStore
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CtutrAttemptsServiceImpl])
trait CtutrAttemptsService {

  def updateAttempts(ctutrAttempts: CtutrAttempts): EitherT[Future, models.Error, CtutrAttempts]

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit]

  def getWithDefault(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, CtutrAttempts]
}

@Singleton
class CtutrAttemptsServiceImpl @Inject() (
  ctutrAttemptsStore: CtutrAttemptsStore,
  timeProvider: TimeProvider,
  config: Configuration
)(implicit
  ec: ExecutionContext
) extends CtutrAttemptsService {

  private val maxCtutrAnswerAttempts: Int        = config.get[Int]("ctutr-attempts.maximum-attempts")
  private val maxCtutrStoreExpiryInSeconds: Long =
    config.get[FiniteDuration]("ctutr-attempts.store-expiry-time").toSeconds

  /**
    * Update & return CtutrAttempts object
    * If maximum attempts reached i.e. lock period is set -> just return fetched data
    * If number of attempts is one less than max allowed -> increment attempts & set the lock period
    * If number of attempts is < (max allowed - 1) -> only increment attempts
    */
  def updateAttempts(ctutrAttempts: CtutrAttempts): EitherT[Future, models.Error, CtutrAttempts] =
    if (ctutrAttempts.isBlocked) {
      EitherT.pure[Future, models.Error](ctutrAttempts)
    } else {
      val lockedUntilOpt  = if (ctutrAttempts.attempts >= maxCtutrAnswerAttempts - 1) {
        Some(timeProvider.now.plusSeconds(maxCtutrStoreExpiryInSeconds))
      } else None
      val updatedAttempts = ctutrAttempts.copy(attempts = ctutrAttempts.attempts + 1, blockedUntil = lockedUntilOpt)
      ctutrAttemptsStore.store(updatedAttempts).map(_ => updatedAttempts)
    }

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit] =
    ctutrAttemptsStore.delete(crn, ggCredId)

  /**
    * Fetch CTUTR attempts using CRN and GGCredId, if none found, return default with number of attempts set to 0
    */
  def getWithDefault(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, CtutrAttempts] =
    ctutrAttemptsStore.get(crn, ggCredId) map {
      case None           => CtutrAttempts(crn, ggCredId, 0, None)
      case Some(attempts) => attempts
    }
}
