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
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.models
import uk.gov.hmrc.hecapplicantfrontend.models.CtutrAttempts
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.repos.CtutrAttemptsStore
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider

import java.time.temporal.ChronoUnit
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CtutrAttemptsServiceImpl])
trait CtutrAttemptsService {

  def createOrIncrementAttempts(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, CtutrAttempts]

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit]

  def get(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Option[CtutrAttempts]]
}

@Singleton
class CtutrAttemptsServiceImpl @Inject() (
  ctutrAttemptsStore: CtutrAttemptsStore,
  timeProvider: TimeProvider,
  appConfig: AppConfig
)(implicit
  ec: ExecutionContext
) extends CtutrAttemptsService {

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
                            val lockedUntilOpt = if (ctutrAttempts.attempts >= appConfig.maxCtutrAnswerAttempts - 1) {
                              val durationInSeconds = appConfig.maxCtutrStoreExpiry.toSeconds
                              Some(timeProvider.now.plus(durationInSeconds, ChronoUnit.SECONDS))
                            } else None
                            ctutrAttempts.copy(attempts = ctutrAttempts.attempts + 1, blockedUntil = lockedUntilOpt)
                        }
      _              <- if (attemptsOpt.contains(updatedAttempts)) EitherT.pure[Future, models.Error](())
                        else ctutrAttemptsStore.store(updatedAttempts)
    } yield updatedAttempts

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit] =
    ctutrAttemptsStore.delete(crn, ggCredId)

  def get(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Option[CtutrAttempts]] =
    ctutrAttemptsStore.get(crn, ggCredId)

}
