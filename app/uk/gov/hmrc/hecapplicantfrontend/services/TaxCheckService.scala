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
import cats.instances.future._
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import play.mvc.Http.Status.CREATED
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import uk.gov.hmrc.hecapplicantfrontend.connectors.HECConnector
import uk.gov.hmrc.hecapplicantfrontend.models.ApplicantDetails.IndividualApplicantDetails
import uk.gov.hmrc.hecapplicantfrontend.models.HECTaxCheckData.IndividualHECTaxCheckData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDetails.IndividualTaxDetails
import uk.gov.hmrc.hecapplicantfrontend.models.{CTStatusResponse, Error, HECTaxCheck, HECTaxCheckData, RetrievedApplicantData, SAStatusResponse, TaxYear}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceDetails
import uk.gov.hmrc.hecapplicantfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[TaxCheckServiceImpl])
trait TaxCheckService {

  def saveTaxCheck(
    retrievedApplicantData: RetrievedApplicantData,
    answers: CompleteUserAnswers
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HECTaxCheck]

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit hc: HeaderCarrier): EitherT[Future, Error, SAStatusResponse]

  def getCTStatus(ctutr: CTUTR, from: LocalDate, to: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, CTStatusResponse]

}

@Singleton
class TaxCheckServiceImpl @Inject() (hecConnector: HECConnector)(implicit ec: ExecutionContext)
    extends TaxCheckService {

  def saveTaxCheck(
    retrievedApplicantData: RetrievedApplicantData,
    answers: CompleteUserAnswers
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HECTaxCheck] = {
    val taxCheckData = toHECTaxCheckData(retrievedApplicantData, answers)
    hecConnector
      .saveTaxCheck(taxCheckData)
      .subflatMap { response =>
        if (response.status =!= CREATED)
          Left(Error(s"Call to save check came back with status ${response.status}"))
        else
          response.parseJSON[HECTaxCheck].leftMap(Error(_))
      }
  }

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, SAStatusResponse] =
    hecConnector
      .getSAStatus(sautr, taxYear)
      .subflatMap { response =>
        if (response.status =!= OK)
          Left(Error(s"Call to get SA status came back with status ${response.status}. Body is ${response.body}"))
        else
          response.parseJSON[SAStatusResponse].leftMap(Error(_))
      }

  def getCTStatus(ctutr: CTUTR, from: LocalDate, to: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, CTStatusResponse] =
    hecConnector
      .getCTStatus(ctutr, from, to)
      .subflatMap { response =>
        if (response.status =!= OK)
          Left(Error(s"Call to get CT status came back with status ${response.status}. Body is ${response.body}"))
        else
          response.parseJSON[CTStatusResponse].leftMap(Error(_))
      }

  private def toHECTaxCheckData(
    retrievedApplicantData: RetrievedApplicantData,
    answers: CompleteUserAnswers
  ): HECTaxCheckData = {
    val licenceDetails = LicenceDetails(
      answers.licenceType,
      answers.licenceExpiryDate,
      answers.licenceTimeTrading,
      answers.licenceValidityPeriod
    )

    retrievedApplicantData match {
      case individual: IndividualRetrievedData =>
        val applicantDetails = IndividualApplicantDetails(
          individual.ggCredId,
          individual.name,
          individual.dateOfBirth
        )

        val taxDetails = IndividualTaxDetails(
          individual.nino,
          individual.sautr,
          answers.taxSituation
        )
        IndividualHECTaxCheckData(applicantDetails, licenceDetails, taxDetails)

      case _: CompanyRetrievedData =>
        sys.error("Not handled yet")
    }

  }

}
