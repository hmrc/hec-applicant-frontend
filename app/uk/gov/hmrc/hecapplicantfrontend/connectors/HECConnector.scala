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

package uk.gov.hmrc.hecapplicantfrontend.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECTaxCheckData, TaxYear}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[HECConnectorImpl])
trait HECConnector {

  def saveTaxCheck(taxCheckData: HECTaxCheckData)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getCTStatus(ctutr: CTUTR, from: LocalDate, to: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

}

@Singleton
class HECConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends HECConnector {

  private val baseUrl: String = servicesConfig.baseUrl("hec")

  private val saveTaxCheckUrl: String = s"$baseUrl/hec/tax-check"

  private def saStatusUrl(sautr: SAUTR, taxYear: TaxYear): String =
    s"$baseUrl/hec/sa-status/${sautr.value}/${taxYear.year}"

  private def ctStatusUrl(ctutr: CTUTR, from: LocalDate, to: LocalDate): String =
    s"$baseUrl/hec/ct-status/${ctutr.value}/${toUrlString(from)}/${toUrlString(to)}"

  private def toUrlString(d: LocalDate): String =
    d.format(DateTimeFormatter.ISO_LOCAL_DATE)

  override def saveTaxCheck(
    taxCheckData: HECTaxCheckData
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .POST[HECTaxCheckData, HttpResponse](saveTaxCheckUrl, taxCheckData)
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](saStatusUrl(sautr, taxYear))
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def getCTStatus(ctutr: CTUTR, from: LocalDate, to: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](ctStatusUrl(ctutr, from, to))
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

}
