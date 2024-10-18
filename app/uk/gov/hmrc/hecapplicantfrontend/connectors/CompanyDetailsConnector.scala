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
import com.google.inject.{ImplementedBy, Singleton}
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.http.HttpReads.Implicits._
import uk.gov.hmrc.http.{HeaderCarrier, HttpClient, HttpResponse}
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import javax.inject.Inject
import scala.concurrent.duration._
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

  // Retry function to handle status code 500 when an end user fails in submitting Company Registration Number
  private def retry[T](
    block: => EitherT[Future, Error, T],
    delay: FiniteDuration,
    maxRetries: Int
  ): EitherT[Future, Error, T] = {
    def attempt(attemptNumber: Int): EitherT[Future, Error, T] =
      block.recoverWith {
        case _ if attemptNumber < maxRetries =>
          // Retry on failure after waiting for 'delay'
          EitherT(Future {
            Thread.sleep(delay.toMillis)
            attempt(attemptNumber + 1).value
          }.flatten)
      }
    // Start the retry attempts after exceeding limit of 'maxRetries'
    attempt(0)
  }

//  override def findCompany(companyNumber: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] =
//    EitherT[Future, Error, HttpResponse](
//      http
//        .GET[HttpResponse](getDetailsUrl(companyNumber))
//        .map(Right(_))
//        .recover { case e => Left(Error(e)) }
//    )

  // TODO: confirm the values of 'retryDelay' and 'maxRetries'
  override def findCompany(companyNumber: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, HttpResponse] = {
    val retryDelay = 1000.milliseconds
    val maxRetries = 3

    retry(
      EitherT(http.GET[HttpResponse](getDetailsUrl(companyNumber)).map(Right(_)).recover { case e: Throwable =>
        Left(Error(e.getMessage))
      }),
      retryDelay,
      maxRetries
    ).leftMap {
      // TODO: confirm with BA/Designers the message we want to show to the end-user when the service fails to submit the company registration number
      // After all retries, if it still fails, return a user-friendly error
      case _: Error =>
        Error("Sorry, the service is not available. Try again in an hour")
    }
  }
}
