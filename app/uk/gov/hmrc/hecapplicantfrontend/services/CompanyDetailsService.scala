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
import cats.instances.int._
import cats.instances.future._
import cats.syntax.either._
import cats.syntax.eq._
import play.api.http.Status.OK
import com.google.inject.{ImplementedBy, Inject}
import uk.gov.hmrc.hecapplicantfrontend.connectors.CompanyDetailsConnector
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyDetails, Error}
import uk.gov.hmrc.hecapplicantfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import javax.inject.Singleton
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[CompanyDetailsServiceImpl])
trait CompanyDetailsService {
  def findCompany(companyNumber: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, CompanyDetails]
}

@Singleton
class CompanyDetailsServiceImpl @Inject() (
  companyDetailsConnector: CompanyDetailsConnector
)(implicit ec: ExecutionContext)
    extends CompanyDetailsService {
  override def findCompany(companyNumber: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, CompanyDetails] =
    companyDetailsConnector.findCompany(companyNumber).subflatMap { httpResponse =>
      if (httpResponse.status =!= OK)
        Left(Error(s"Response to get company details came back with status ${httpResponse.status}"))
      else
        httpResponse
          .parseJSON[CompanyDetails]
          .leftMap(Error(_))
    }

}
