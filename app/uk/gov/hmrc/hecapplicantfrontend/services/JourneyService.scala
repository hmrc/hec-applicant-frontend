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

import cats.Eq
import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.mvc.Call
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.saTaxSituations
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.{Company, Individual}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.SAStatus.ReturnFound
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.{EntityType, Error, HECSession, IncomeDeclared, RetrievedApplicantData, SAStatus, SAStatusResponse, TaxSituation, UserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyServiceImpl._
import uk.gov.hmrc.http.HeaderCarrier

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[JourneyServiceImpl])
trait JourneyService {

  def updateAndNext(current: Call, updatedSession: HECSession)(implicit
    r: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Call]

  def previous(current: Call)(implicit r: RequestWithSessionData[_]): Call

  def firstPage(session: HECSession): Call

}

@Singleton
class JourneyServiceImpl @Inject() (sessionStore: SessionStore)(implicit ex: ExecutionContext) extends JourneyService {

  implicit val callEq: Eq[Call] = Eq.instance(_.url === _.url)

  // map representing routes from one page to another when users submit answers. The keys are the current page and the
  // values are the destination pages which come after the current page. The destination can sometimes depend
  // on state (e.g. the type of user or the answers users have submitted), hence the value type `HECSession => Call`
  lazy val paths: Map[Call, HECSession => Call] = Map(
    routes.StartController.start()                                       -> firstPage,
    routes.ConfirmIndividualDetailsController.confirmIndividualDetails() -> (_ =>
      routes.LicenceDetailsController.licenceType()
    ),
    routes.LicenceDetailsController.licenceType()                        -> (_ => routes.LicenceDetailsController.licenceTimeTrading()),
    routes.LicenceDetailsController.licenceTimeTrading                   -> (_ => routes.LicenceDetailsController.recentLicenceLength()),
    routes.LicenceDetailsController.recentLicenceLength()                -> licenceValidityPeriodRoute,
    routes.EntityTypeController.entityType()                             -> entityTypeRoute,
    routes.TaxSituationController.taxSituation()                         -> taxSituationRoute,
    routes.SAController.saIncomeStatement()                              -> (_ => routes.CheckYourAnswersController.checkYourAnswers()),
    routes.CheckYourAnswersController.checkYourAnswers()                 -> (_ => routes.TaxCheckCompleteController.taxCheckComplete()),
    routes.CRNController.companyRegistrationNumber()                     -> companyRegistrationNumberRoute
  )

  // map which describes routes from an exit page to their previous page. The keys are the exit page and the values are
  // the pages previous to them. These routes are ones which should not be described in `path` as they are typically not
  // triggered by a submit, but rather by clicking a link on the source page for instance.
  lazy val exitPageToPreviousPage: Map[Call, Call] =
    Map(
      routes.ConfirmIndividualDetailsController
        .confirmIndividualDetailsExit()                 -> routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
      routes.LicenceDetailsController.licenceTypeExit() ->
        routes.LicenceDetailsController.licenceType(),
      routes.EntityTypeController.wrongEntityType()     ->
        routes.EntityTypeController.entityType()
    )

  override def firstPage(session: HECSession): Call =
    session.retrievedUserData match {
      case _: IndividualRetrievedData => routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
      case _: CompanyRetrievedData    => routes.LicenceDetailsController.licenceType()
    }

  override def updateAndNext(current: Call, updatedSession: HECSession)(implicit
    r: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Call] =
    for {
      upliftedSession <- EitherT.fromEither[Future](upliftToCompleteAnswersIfComplete(updatedSession, current))
      next            <- EitherT.fromOption[Future](
                           upliftedSession.userAnswers.fold(
                             _ => paths.get(current).map(_(upliftedSession)),
                             _ => Some(routes.CheckYourAnswersController.checkYourAnswers())
                           ),
                           Error(s"Could not find next for $current")
                         )
      _               <- if (r.sessionData === upliftedSession)
                           EitherT.pure[Future, Error](next)
                         else
                           sessionStore.store(upliftedSession).map(_ => next)
    } yield next

  override def previous(current: Call)(implicit
    r: RequestWithSessionData[_]
  ): Call = {
    @tailrec
    def loop(previous: Call): Option[Call] =
      paths.get(previous) match {
        case Some(calculateNext) =>
          val next = calculateNext(r.sessionData)
          if (next === current) Some(previous)
          else loop(next)
        case _                   => None
      }

    lazy val hasCompletedAnswers = r.sessionData.userAnswers.fold(_ => false, _ => true)

    if (current === routes.StartController.start())
      current
    else if (current =!= routes.CheckYourAnswersController.checkYourAnswers() && hasCompletedAnswers)
      routes.CheckYourAnswersController.checkYourAnswers()
    else
      exitPageToPreviousPage
        .get(current)
        .orElse(loop(routes.StartController.start()))
        .getOrElse(sys.error(s"Could not find previous for $current"))
  }

  private def upliftToCompleteAnswersIfComplete(session: HECSession, current: Call): Either[Error, HECSession] =
    paths.get(current).map(_(session)) match {
      case None =>
        Left(Error(s"Could not find next for $current"))

      case Some(next) =>
        // if we're not on the last page and there is no next page some exit page has been reached
        val isExitPageNext =
          !paths.contains(next) && next =!= routes.TaxCheckCompleteController.taxCheckComplete()

        val updatedSession = session.userAnswers match {
          case _ if isExitPageNext =>
            session
          //will add a case for when company when it reaches the complete answers page
          //if it's  added now, the code thinks the company's answers are all complete and take it to check your answers page
          case incomplete @ IncompleteUserAnswers(
                Some(licenceType),
                Some(licenceTimeTrading),
                Some(licenceValidityPeriod),
                taxSituation,
                saIncomeDeclared,
                entityType,
                crn
              ) if allAnswersComplete(incomplete, session.retrievedUserData) =>
            val completeAnswers =
              CompleteUserAnswers(
                licenceType,
                licenceTimeTrading,
                licenceValidityPeriod,
                taxSituation,
                saIncomeDeclared,
                entityType,
                crn
              )
            session.copy(userAnswers = completeAnswers)

          case _ => session
        }

        Right(updatedSession)
    }

  private def licenceValidityPeriodRoute(session: HECSession): Call =
    session.userAnswers.fold(_.licenceType, c => Some(c.licenceType)) match {
      case Some(licenceType) =>
        if (licenceTypeForIndividualAndCompany(licenceType)) routes.EntityTypeController.entityType()
        else routes.TaxSituationController.taxSituation()
      case None              =>
        sys.error("Could not find licence type to work out route after licence validity period")
    }

  private def entityTypeRoute(session: HECSession): Call = {
    val maybeSelectedEntityType = session.userAnswers.fold(_.entityType, _.entityType)
    val ggEntityType            = EntityType.fromRetrievedApplicantAnswers(session.retrievedUserData)

    maybeSelectedEntityType match {
      case None                     =>
        sys.error("Could not find selected entity type for entity type route")
      case Some(selectedEntityType) =>
        if (selectedEntityType === ggEntityType && selectedEntityType === Individual)
          routes.TaxSituationController.taxSituation()
        else if (selectedEntityType === ggEntityType && selectedEntityType === Company)
          routes.CRNController.companyRegistrationNumber()
        else routes.EntityTypeController.wrongGGAccount()
    }

  }

  private def taxSituationRoute(session: HECSession): Call =
    UserAnswers.taxSituation(session.userAnswers) match {
      case None               =>
        sys.error("Could not find tax situation for tax situation route")
      case Some(taxSituation) =>
        if (saTaxSituations.contains(taxSituation)) {
          session.retrievedUserData match {
            case IndividualRetrievedData(_, _, Some(_), _, _, _, Some(saStatus)) =>
              saStatus.status match {
                case SAStatus.ReturnFound        => routes.SAController.saIncomeStatement()
                case SAStatus.NoticeToFileIssued => routes.CheckYourAnswersController.checkYourAnswers()
                case SAStatus.NoReturnFound      => routes.SAController.noReturnFound()
              }

            case i: IndividualRetrievedData if i.sautr.isDefined && i.saStatus.isEmpty =>
              sys.error("Found SA UTR for tax situation route but no SA status response")

            case i: IndividualRetrievedData if i.sautr.isEmpty =>
              routes.SAController.sautrNotFound()

            case _: CompanyRetrievedData =>
              sys.error("Retrieved data for company found for tax situation route")
          }
        } else {
          routes.CheckYourAnswersController.checkYourAnswers()
        }
    }

  private def companyRegistrationNumberRoute(session: HECSession) =
    session.retrievedUserData match {
      case _: IndividualRetrievedData             =>
        sys.error("This may never happen, Individual data shouldn't be present  in company journey")
      case CompanyRetrievedData(_, _, _, Some(_)) => routes.CompanyDetailsController.confirmCompanyDetails()
      case _                                      => routes.CompanyDetailsNotFoundController.companyNotFound()
    }

}

object JourneyServiceImpl {
  def licenceTypeForIndividualAndCompany(licenceType: LicenceType): Boolean = licenceType match {
    case LicenceType.DriverOfTaxisAndPrivateHires => false
    case _                                        => true
  }

  /**
    * Expect the entity type to be specified only for individual or company licence types
    */
  private def checkEntityTypePresentIfRequired(licenceType: LicenceType, entityType: Option[EntityType]): Boolean =
    entityType match {
      case Some(_) if licenceTypeForIndividualAndCompany(licenceType) => true
      case None if !licenceTypeForIndividualAndCompany(licenceType)   => true
      case _                                                          => false
    }

  /**
    * Expect the SA income to be declared only for an SA tax situation whose return has been found
    */
  private def checkSAIncomeDeclared(
    taxSituation: TaxSituation,
    saIncomeDeclared: Option[IncomeDeclared],
    retrievedUserData: RetrievedApplicantData
  ): Boolean =
    (retrievedUserData, saIncomeDeclared) match {
      case (IndividualRetrievedData(_, _, _, _, _, _, Some(SAStatusResponse(_, _, ReturnFound))), None)
          if saTaxSituations.contains(taxSituation) =>
        false
      case _ => true
    }

  /**
    * Process the incomplete answers and retrieved user data to determine if all answers have been given by the user
    * @param incompleteUserAnswers The incomplete answers
    * @param retrievedUserData The retrieved user data
    * @return A boolean representing whether or not the user has completed answering all relevant questions
    */
  def allAnswersComplete(
    incompleteUserAnswers: IncompleteUserAnswers,
    retrievedUserData: RetrievedApplicantData
  ): Boolean = {
    val isIndividual = retrievedUserData match {
      case _: IndividualRetrievedData => true
      case _: CompanyRetrievedData    => false
    }
    incompleteUserAnswers match {
      case IncompleteUserAnswers(
            Some(licenceType),
            Some(_),
            Some(_),
            Some(taxSituation),
            saIncomeDeclared,
            entityType,
            _
          ) if isIndividual =>
        val licenceTypeCheck      = checkEntityTypePresentIfRequired(licenceType, entityType)
        val saIncomeDeclaredCheck = checkSAIncomeDeclared(taxSituation, saIncomeDeclared, retrievedUserData)
        licenceTypeCheck && saIncomeDeclaredCheck

      //TODO add company scenario later when it reaches the check your answer page
      case _ => false
    }
  }
}
