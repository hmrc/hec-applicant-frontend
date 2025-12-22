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
import cats.instances.int.*
import cats.syntax.eq.*
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.CREATED
import uk.gov.hmrc.hecapplicantfrontend.testonly.connectors.HECConnector
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.SaveTaxCheckRequest
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[HECServiceImpl])
trait HECService {

  def saveTaxCheck(saveTaxCheckRequest: SaveTaxCheckRequest)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

}

@Singleton
class HECServiceImpl @Inject() (connector: HECConnector)(implicit ec: ExecutionContext) extends HECService {

  def saveTaxCheck(saveTaxCheckRequest: SaveTaxCheckRequest)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    connector.saveTaxCheck(saveTaxCheckRequest).subflatMap { httpResponse =>
      val status = httpResponse.status

      if (status =!= CREATED)
        Left(Error(s"Call to save tax check came back with status $status. Body was ${httpResponse.body}"))
      else
        Right(())
    }

}
