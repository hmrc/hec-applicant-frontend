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

  def createOrIncrementAttempts(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, CtutrAttempts]

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit]
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
    * Fetch CTUTR attempts for the CRN-GGCredId combination
    * If not found, create a new document with 1 attempt
    * If found & maximum attempts reached i.e. lock period is set, just return fetched data
    * If found & number of attempts one less than max allowed, increment attempts & set the lock period
    * If found & number of attempts more than one less than max allowed, only increment attempts
    * @return The updated CTUTR attempts
    */
  def createOrIncrementAttempts(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, CtutrAttempts] =
    for {
      attemptsOpt    <- ctutrAttemptsStore.get(crn, ggCredId)
      updatedAttempts = attemptsOpt match {
                          case None                                           => CtutrAttempts(crn, ggCredId, 1, None)
                          case Some(ctutrAttempts) if ctutrAttempts.isBlocked => ctutrAttempts
                          case Some(ctutrAttempts)                            =>
                            val lockedUntilOpt = if (ctutrAttempts.attempts >= maxCtutrAnswerAttempts - 1) {
                              Some(timeProvider.now.plusSeconds(maxCtutrStoreExpiryInSeconds))
                            } else None
                            ctutrAttempts.copy(attempts = ctutrAttempts.attempts + 1, blockedUntil = lockedUntilOpt)
                        }
      _              <- if (attemptsOpt.contains(updatedAttempts)) EitherT.pure[Future, models.Error](())
                        else ctutrAttemptsStore.store(updatedAttempts)
    } yield updatedAttempts

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit] =
    ctutrAttemptsStore.delete(crn, ggCredId)
}
