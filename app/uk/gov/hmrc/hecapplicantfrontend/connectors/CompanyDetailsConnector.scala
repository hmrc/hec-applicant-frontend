/*
 * Copyright 2022 HM Revenue & Customs
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
import com.google.inject.{ImplementedBy, Singleton}
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CompanyDetailsConnectorImpl])
trait CompanyDetailsConnector {

  def findCompany(companyNumber: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse]

}

@Singleton
class CompanyDetailsConnectorImpl @Inject() (http: HttpClient, servicesConfig: ServicesConfig)(implicit
  ec: ExecutionContext
) extends CompanyDetailsConnector {

  private val baseUrl: String = s"${servicesConfig.baseUrl("companies-house-proxy")}"

  private def getDetailsUrl(companyNumber: CRN): String =
    s"$baseUrl/companies-house-api-proxy/company/${companyNumber.value}"

  override def findCompany(companyNumber: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
    EitherT[Future, Error, HttpResponse](
      http
        .GET[HttpResponse](getDetailsUrl(companyNumber))
        .map(Right(_))
        .recover { case e => Left(Error(e)) }
    )
}
