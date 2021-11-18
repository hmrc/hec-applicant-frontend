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
import cats.syntax.eq._
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

  def updateAttempts(
    crn: CRN,
    ggCredId: GGCredId,
    ctutrAttempts: Option[CtutrAttempts]
  ): EitherT[Future, models.Error, CtutrAttempts]

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit]

  def get(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Option[CtutrAttempts]]
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
    * Update a previously fetched CtutrAttempts object
    * Note: crn and ggCredId values should match corresponding values in ctutrAttempts if defined
    *
    * If ctutrAttempts
    *    is undefined -> create a new document with 1 attempt
    *    is defined & maximum attempts reached i.e. lock period is set -> just return fetched data
    *    is defined & number of attempts one less than max allowed -> increment attempts & set the lock period
    *    is defined & number of attempts more than one less than max allowed -> only increment attempts
    *
    * @param crn The CRN value
    * @param ggCredId The GGCredId
    * @param ctutrAttempts Previously fetched CTUTR attempts
    * @return
    */
  def updateAttempts(
    crn: CRN,
    ggCredId: GGCredId,
    ctutrAttempts: Option[CtutrAttempts]
  ): EitherT[Future, models.Error, CtutrAttempts] = {
    require(
      ctutrAttempts.forall(ca => ca.crn === crn && ca.ggCredId === ggCredId),
      "crn and ggCredId should match corresponding values in ctutrAttempts if defined"
    )

    val updatedAttempts = ctutrAttempts match {
      case None                                           => CtutrAttempts(crn, ggCredId, 1, None)
      case Some(ctutrAttempts) if ctutrAttempts.isBlocked => ctutrAttempts
      case Some(ctutrAttempts)                            =>
        val lockedUntilOpt = if (ctutrAttempts.attempts >= maxCtutrAnswerAttempts - 1) {
          Some(timeProvider.now.plusSeconds(maxCtutrStoreExpiryInSeconds))
        } else None
        ctutrAttempts.copy(attempts = ctutrAttempts.attempts + 1, blockedUntil = lockedUntilOpt)
    }

    if (ctutrAttempts.contains(updatedAttempts))
      EitherT.pure[Future, models.Error](updatedAttempts)
    else
      ctutrAttemptsStore.store(updatedAttempts).map(_ => updatedAttempts)
  }

  def delete(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Unit] =
    ctutrAttemptsStore.delete(crn, ggCredId)

  def get(crn: CRN, ggCredId: GGCredId): EitherT[Future, models.Error, Option[CtutrAttempts]] =
    ctutrAttemptsStore.get(crn, ggCredId)
}
