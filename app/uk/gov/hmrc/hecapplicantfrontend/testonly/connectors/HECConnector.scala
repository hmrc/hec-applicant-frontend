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

package uk.gov.hmrc.hecapplicantfrontend.testonly.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.SaveTaxCheckRequest
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.http.client.HttpClientV2
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal

@ImplementedBy(classOf[HECConnectorImpl])
trait HECConnector {
  def saveTaxCheck(saveTaxCheckRequest: SaveTaxCheckRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class HECConnectorImpl @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends HECConnector {

  private val baseUrl: String = servicesConfig.baseUrl("hec")

  private val saveTaxCheckUrl: String = s"$baseUrl/hec/test-only/tax-check"

  def saveTaxCheck(
    saveTaxCheckRequest: SaveTaxCheckRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(url"$saveTaxCheckUrl")
        .withBody(Json.toJson(saveTaxCheckRequest))
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case NonFatal(e) =>
          Left(Error(e))
        }
    )

}
