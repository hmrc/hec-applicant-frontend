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
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models
import uk.gov.hmrc.hecapplicantfrontend.models.CtutrAttempts
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.repos.CtutrAttemptsStore
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CtutrAttemptsServiceImpl])
trait CtutrAttemptsService {

  def fetchAndUpdateFor(crn: CRN, ggCredId: GGCredId)(implicit
    request: RequestWithSessionData[_]
  ): EitherT[Future, models.Error, CtutrAttempts]
}

@Singleton
class CtutrAttemptsServiceImpl @Inject() (
  ctutrAttemptsStore: CtutrAttemptsStore,
  timeProvider: TimeProvider
)(implicit
  ec: ExecutionContext
) extends CtutrAttemptsService {

  /**
    * Fetch CTUTR attempts for the CRN-GGCredId combination
    * If found, increment the number of attempts
    * If not found, create a new document with 0 attempts
    * Store the new/updated data back into the database
    * @return The updated CTUTR attempts
    */
  def fetchAndUpdateFor(crn: CRN, ggCredId: GGCredId)(implicit
    request: RequestWithSessionData[_]
  ): EitherT[Future, models.Error, CtutrAttempts] =
    for {
      attemptsOpt    <- ctutrAttemptsStore.get(crn, ggCredId)
      updatedAttempts = attemptsOpt match {
                          case Some(ctutrAttempts) =>
                            ctutrAttempts
                              .copy(attempts = ctutrAttempts.attempts + 1, lastUpdated = timeProvider.now)
                          case None                =>
                            CtutrAttempts(crn, ggCredId, 0, timeProvider.now)
                        }
      _              <- ctutrAttemptsStore.store(updatedAttempts)
    } yield updatedAttempts

}
