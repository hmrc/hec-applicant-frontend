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
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.CompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.ApplicantDetails.{CompanyApplicantDetails, IndividualApplicantDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.HECTaxCheckData.{CompanyHECTaxCheckData, IndividualHECTaxCheckData}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.TaxDetails.{CompanyTaxDetails, IndividualTaxDetails}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTStatusResponse
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.SAStatusResponse
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.{HECTaxCheckData, HECTaxCheckSource}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceDetails
import uk.gov.hmrc.hecapplicantfrontend.models.{CompleteUserAnswers, Error, HECSession, HECTaxCheck, Language, TaxCheckListItem, TaxYear}
import uk.gov.hmrc.hecapplicantfrontend.services.TaxCheckService._
import uk.gov.hmrc.hecapplicantfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[TaxCheckServiceImpl])
trait TaxCheckService {

  def saveTaxCheck(
    session: HECSession,
    answers: CompleteUserAnswers,
    languagePreference: Language
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HECTaxCheck]

  def getSAStatus(sautr: SAUTR, taxYear: TaxYear)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[SAStatusResponse]]

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
    answers: CompleteUserAnswers,
    languagePreference: Language
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, HECTaxCheck] = {
    val taxCheckData = toHECTaxCheckData(session, answers, languagePreference)
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
  ): EitherT[Future, Error, Option[SAStatusResponse]] =
    hecConnector
      .getSAStatus(sautr, taxYear)
      .subflatMap { response =>
        response.status match {
          case NOT_FOUND               => Right(None)
          case status if status =!= OK =>
            Left(Error(s"Call to get SA status came back with status ${response.status}. Body is ${response.body}"))
          case _                       =>
            response.parseJSON[SAStatusResponse].leftMap(Error(_)).map(Some(_))
        }
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
    completeUserAnswers: CompleteUserAnswers,
    languagePreference: Language
  ): HECTaxCheckData =
    (session, completeUserAnswers) match {
      case (
            IndividualHECSession(
              loginData,
              retrievedJourneyData,
              _,
              _,
              taxCheckStartDateTime,
              _,
              _,
              relevantIncomeTaxYear,
              _,
              _,
              _,
              _,
              _
            ),
            answers: CompleteIndividualUserAnswers
          ) =>
        val licenceDetails   = LicenceDetails(
          answers.licenceType,
          answers.licenceTimeTrading,
          answers.licenceValidityPeriod
        )
        val applicantDetails = IndividualApplicantDetails(
          loginData.ggCredId,
          loginData.name,
          loginData.dateOfBirth
        )

        val taxDetails = IndividualTaxDetails(
          loginData.nino,
          loginData.sautr,
          answers.taxSituation,
          answers.saIncomeDeclared,
          retrievedJourneyData.saStatus,
          relevantIncomeTaxYear.getOrElse(
            sys.error("relevant Income tax year is not present")
          )
        )
        IndividualHECTaxCheckData(
          applicantDetails,
          licenceDetails,
          taxDetails,
          taxCheckStartDateTime.getOrElse(
            sys.error("taxCheckStartDateTime is not present")
          ),
          HECTaxCheckSource.Digital,
          languagePreference,
          loginData.didConfirmUncertainEntityType,
          filterFromFileTransfer = None
        )

      case (
            CompanyHECSession(
              companyLoginData,
              retrievedJourneyData,
              _,
              _,
              taxCheckStartDateTime,
              _,
              _,
              _,
              _,
              _,
              _,
              _
            ),
            answers: CompleteCompanyUserAnswers
          ) =>
        val companyApplicantDetails = CompanyApplicantDetails(
          companyLoginData.ggCredId,
          answers.crn,
          retrievedJourneyData.companyName.getOrElse(sys.error("company House Name is not present"))
        )

        val companyLicenceDetails = LicenceDetails(
          answers.licenceType,
          answers.licenceTimeTrading,
          answers.licenceValidityPeriod
        )

        val companyTaxDetails = CompanyTaxDetails(
          retrievedJourneyData.desCtutr.getOrElse(sys.error("CTUTR not found")),
          answers.ctutr,
          answers.ctIncomeDeclared,
          retrievedJourneyData.ctStatus.getOrElse(sys.error("ct status response not found")),
          answers.recentlyStartedTrading,
          answers.chargeableForCT
        )

        CompanyHECTaxCheckData(
          companyApplicantDetails,
          companyLicenceDetails,
          companyTaxDetails,
          taxCheckStartDateTime.getOrElse(
            sys.error("taxCheckStartDateTime is not present")
          ),
          HECTaxCheckSource.Digital,
          languagePreference,
          companyLoginData.didConfirmUncertainEntityType,
          filterFromFileTransfer = None
        )

      case _ => sys.error("Invalid session & complete answers combination")
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
