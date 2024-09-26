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

package uk.gov.hmrc.hecapplicantfrontend.connectors

import cats.data.EitherT
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.{HECTaxCheckData, SaveEmailAddressRequest}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, TaxYear}
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse, StringContextOps}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig
import uk.gov.hmrc.http.client.HttpClientV2
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[HECConnectorImpl])
trait HECConnector {

  def saveTaxCheck(taxCheckData: HECTaxCheckData)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getCTStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]

  def getCtutr(crn: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def getUnexpiredTaxCheckCodes()(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

  def saveEmailAddress(saveEmailAddressRequest: SaveEmailAddressRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse]
}

@Singleton
class HECConnectorImpl @Inject() (http: HttpClientV2, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends HECConnector {

  private val baseUrl: String = servicesConfig.baseUrl("hec")

  private val saveTaxCheckUrl: String = s"$baseUrl/hec/tax-check"

  private def toUrlString(d: LocalDate): String = d.format(DateTimeFormatter.ISO_LOCAL_DATE)

  private val getTaxCheckCodesUrl: String = s"$baseUrl/hec/unexpired-tax-checks"

  private val saveEmailAddressUrl: String = s"$baseUrl/hec/email-address"

  override def saveTaxCheck(
    taxCheckData: HECTaxCheckData
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(url"$saveTaxCheckUrl")
        .withBody(Json.toJson(taxCheckData))
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(url"$baseUrl/hec/sa-status/$sautr/$taxYear")
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def getCTStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(url"$baseUrl/hec/ct-status/${ctutr.value}/${toUrlString(startDate)}/${toUrlString(endDate)}")
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def getCtutr(crn: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(url"$baseUrl/hec/ctutr/$crn")
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def getUnexpiredTaxCheckCodes()(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .get(url"$getTaxCheckCodesUrl")
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )

  def saveEmailAddress(
    saveEmailAddressRequest: SaveEmailAddressRequest
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .post(url"$saveEmailAddressUrl")
        .withBody(Json.toJson(saveEmailAddressRequest))
        .execute[HttpResponse]
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
}
