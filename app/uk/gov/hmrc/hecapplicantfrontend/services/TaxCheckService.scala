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
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.OK
import play.api.libs.json.{Json, OFormat}
import play.mvc.Http.Status.{CREATED, NOT_FOUND}
import uk.gov.hmrc.hecapplicantfrontend.connectors.HECConnector
import uk.gov.hmrc.hecapplicantfrontend.models.ApplicantDetails.{CompanyApplicantDetails, IndividualApplicantDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.HECTaxCheckData.{CompanyHECTaxCheckData, IndividualHECTaxCheckData}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDetails.{CompanyTaxDetails, IndividualTaxDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceDetails
import uk.gov.hmrc.hecapplicantfrontend.models.{CTStatusResponse, Error, HECSession, HECTaxCheck, HECTaxCheckData, HECTaxCheckSource, SAStatusResponse, TaxCheckListItem, TaxYear}
import uk.gov.hmrc.hecapplicantfrontend.services.TaxCheckService._
import uk.gov.hmrc.hecapplicantfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[TaxCheckServiceImpl])
trait TaxCheckService {

  def saveTaxCheck(
    session: HECSession,
    answers: CompleteUserAnswers
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HECTaxCheck]

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit hc: HeaderCarrier): EitherT[Future, Error, SAStatusResponse]

  def getCTStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[CTStatusResponse]]

  def getCtutr(crn: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[CTUTR]]

  def getUnexpiredTaxCheckCodes()(implicit hc: HeaderCarrier): EitherT[Future, Error, List[TaxCheckListItem]]

}

@Singleton
class TaxCheckServiceImpl @Inject() (hecConnector: HECConnector)(implicit ec: ExecutionContext)
    extends TaxCheckService {

  def saveTaxCheck(
    session: HECSession,
    answers: CompleteUserAnswers
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HECTaxCheck] = {
    val taxCheckData = toHECTaxCheckData(session, answers)
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

  def getCTStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[CTStatusResponse]] =
    hecConnector
      .getCTStatus(ctutr, startDate, endDate)
      .subflatMap { response =>
        response.status match {
          case NOT_FOUND               =>
            Right(None)
          case status if status =!= OK =>
            Left(Error(s"Call to get CT status came back with status ${response.status}. Body is ${response.body}"))
          case _                       =>
            response.parseJSON[CTStatusResponse].leftMap(Error(_)).map(Some(_))
        }
      }

  def getCtutr(crn: CRN)(implicit hc: HeaderCarrier): EitherT[Future, Error, Option[CTUTR]] =
    hecConnector
      .getCtutr(crn)
      .subflatMap { response =>
        response.status match {
          case NOT_FOUND               =>
            Right(None)
          case status if status =!= OK =>
            Left(Error(s"Call to get CTUTR came back with status ${response.status}. Body is ${response.body}"))
          case _                       =>
            response.parseJSON[CTUTRFromCRNResponse].map(r => Some(r.ctutr)).leftMap(Error(_))
        }
      }

  private def toHECTaxCheckData(
    session: HECSession,
    completeUserAnswers: CompleteUserAnswers
  ): HECTaxCheckData = session match {
    case IndividualHECSession(
          loginData,
          retrievedJourneyData,
          _,
          _,
          taxCheckStartDateTime,
          _
        ) =>
      val licenceDetails   = LicenceDetails(
        completeUserAnswers.licenceType,
        completeUserAnswers.licenceTimeTrading,
        completeUserAnswers.licenceValidityPeriod
      )
      val applicantDetails = IndividualApplicantDetails(
        loginData.ggCredId,
        loginData.name,
        loginData.dateOfBirth
      )

      val taxDetails = IndividualTaxDetails(
        loginData.nino,
        loginData.sautr,
        completeUserAnswers.taxSituation,
        completeUserAnswers.saIncomeDeclared,
        retrievedJourneyData.saStatus
      )
      IndividualHECTaxCheckData(
        applicantDetails,
        licenceDetails,
        taxDetails,
        taxCheckStartDateTime.getOrElse(
          sys.error("taxCheckStartDateTime is not present")
        ),
        HECTaxCheckSource.Digital
      )

    case CompanyHECSession(
          companyLoginData,
          retrievedJourneyData,
          _,
          _,
          taxCheckStartDateTime,
          _,
          _
        ) =>
      val companyApplicantDetails = CompanyApplicantDetails(
        companyLoginData.ggCredId,
        completeUserAnswers.crn.getOrElse(sys.error("crn is not present")),
        retrievedJourneyData.companyName.getOrElse(sys.error("company House Name is not present"))
      )

      val companyLicenceDetails = LicenceDetails(
        completeUserAnswers.licenceType,
        completeUserAnswers.licenceTimeTrading,
        completeUserAnswers.licenceValidityPeriod
      )

      val companyTaxDetails = CompanyTaxDetails(
        retrievedJourneyData.desCtutr.getOrElse(sys.error("CTUTR not found")),
        completeUserAnswers.ctutr,
        completeUserAnswers.ctIncomeDeclared,
        retrievedJourneyData.ctStatus.getOrElse(sys.error("ct status response not found")),
        completeUserAnswers.recentlyStartedTrading,
        completeUserAnswers.chargeableForCT
      )

      CompanyHECTaxCheckData(
        companyApplicantDetails,
        companyLicenceDetails,
        companyTaxDetails,
        taxCheckStartDateTime.getOrElse(
          sys.error("taxCheckStartDateTime is not present")
        ),
        HECTaxCheckSource.Digital
      )

  }

  def getUnexpiredTaxCheckCodes()(implicit hc: HeaderCarrier): EitherT[Future, Error, List[TaxCheckListItem]] =
    hecConnector
      .getUnexpiredTaxCheckCodes()
      .subflatMap { response =>
        if (response.status =!= OK)
          Left(Error(s"Call to get tax check codes came back with status ${response.status}. Body is ${response.body}"))
        else {
          response.parseJSON[List[TaxCheckListItem]].leftMap(Error(_))
        }
      }

}

object TaxCheckService {
  final case class CTUTRFromCRNResponse(ctutr: CTUTR)
  implicit val format: OFormat[CTUTRFromCRNResponse] = Json.format
}
