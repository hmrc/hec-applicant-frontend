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
import cats.syntax.option._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.mvc.Call
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.saTaxSituations
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.{CompleteCompanyUserAnswers, IncompleteCompanyUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.{Company, Individual}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.{CompleteIndividualUserAnswers, IncompleteIndividualUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.SAStatus.ReturnFound
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.{CTStatus, EntityType, Error, HECSession, SAStatus, SAStatusResponse, TaxSituation, YesNoAnswer}
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
class JourneyServiceImpl @Inject() (sessionStore: SessionStore)(implicit ex: ExecutionContext, appConfig: AppConfig)
    extends JourneyService {

  implicit val callEq: Eq[Call] = Eq.instance(_.url === _.url)

  // map representing routes from one page to another when users submit answers. The keys are the current page and the
  // values are the destination pages which come after the current page. The destination can sometimes depend
  // on state (e.g. the type of user or the answers users have submitted), hence the value type `HECSession => Call`
  lazy val paths: Map[Call, HECSession => Call] = Map(
    routes.StartController.start()                                       -> firstPage,
    routes.ConfirmIndividualDetailsController.confirmIndividualDetails() -> confirmIndividualDetailsRoute,
    routes.TaxChecksListController.unexpiredTaxChecks()                  -> (_ => routes.LicenceDetailsController.licenceType()),
    routes.LicenceDetailsController.licenceType()                        -> (_ => routes.LicenceDetailsController.licenceTimeTrading()),
    routes.LicenceDetailsController.licenceTimeTrading                   -> (_ => routes.LicenceDetailsController.recentLicenceLength()),
    routes.LicenceDetailsController.recentLicenceLength()                -> licenceValidityPeriodRoute,
    routes.EntityTypeController.entityType()                             -> entityTypeRoute,
    routes.TaxSituationController.taxSituation()                         -> taxSituationRoute,
    routes.SAController.saIncomeStatement()                              -> (_ => routes.CheckYourAnswersController.checkYourAnswers()),
    routes.CheckYourAnswersController.checkYourAnswers()                 -> (_ => routes.TaxCheckCompleteController.taxCheckComplete()),
    routes.CRNController.companyRegistrationNumber()                     -> companyRegistrationNumberRoute,
    routes.CompanyDetailsController.confirmCompanyDetails()              -> confirmCompanyDetailsRoute,
    routes.CompanyDetailsController.chargeableForCorporationTax()        -> chargeableForCTRoute,
    routes.CompanyDetailsController.ctIncomeStatement()                  -> (_ => routes.CheckYourAnswersController.checkYourAnswers()),
    routes.CompanyDetailsController.recentlyStartedTrading()             -> recentlyStartedTradingRoute,
    routes.CompanyDetailsController.enterCtutr()                         -> enterCtutrRoute
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
        routes.EntityTypeController.entityType(),
      routes.CompanyDetailsController.dontHaveUtr()     ->
        routes.CompanyDetailsController.enterCtutr()
    )

  override def firstPage(session: HECSession): Call = {
    val hasTaxCheckCodes = session.unexpiredTaxChecks.nonEmpty
    session.loginData match {
      case _: IndividualLoginData                  => routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
      case _: CompanyLoginData if hasTaxCheckCodes => routes.TaxChecksListController.unexpiredTaxChecks()
      case _: CompanyLoginData                     => routes.LicenceDetailsController.licenceType()
    }
  }

  override def updateAndNext(current: Call, updatedSession: HECSession)(implicit
    r: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Call] = {
    val currentPageIsCYA: Boolean = current === routes.CheckYourAnswersController.checkYourAnswers()
    for {
      upliftedSession <- EitherT.fromEither[Future](upliftToCompleteAnswersIfComplete(updatedSession, current))
      next            <- EitherT.fromOption[Future](
                           upliftedSession.userAnswers.foldByCompleteness(
                             _ =>
                               if (currentPageIsCYA) sys.error("All user answers are not complete")
                               else paths.get(current).map(_(upliftedSession)),
                             _ =>
                               if (currentPageIsCYA) paths.get(current).map(_(upliftedSession))
                               else Some(routes.CheckYourAnswersController.checkYourAnswers())
                           ),
                           Error(s"Could not find next for $current")
                         )
      _               <- if (r.sessionData === upliftedSession)
                           EitherT.pure[Future, Error](next)
                         else
                           sessionStore.store(upliftedSession).map(_ => next)
    } yield next
  }

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

    lazy val hasCompletedAnswers = r.sessionData.userAnswers.foldByCompleteness(_ => false, _ => true)

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

        val updatedSession = session match {
          case _ if isExitPageNext => session

          case companySession @ CompanyHECSession(
                _,
                _,
                companyAnswers @ IncompleteCompanyUserAnswers(
                  Some(licenceType),
                  Some(licenceTimeTrading),
                  Some(licenceValidityPeriod),
                  Some(entityType),
                  Some(crn),
                  Some(companyDetailsConfirmed),
                  chargeableForCT,
                  ctIncomeDeclared,
                  recentlyStartedTrading,
                  ctutr
                ),
                _,
                _,
                _,
                _
              ) if allCompanyAnswersComplete(companyAnswers, companySession) =>
            val completeAnswers =
              CompleteCompanyUserAnswers(
                licenceType,
                licenceTimeTrading,
                licenceValidityPeriod,
                entityType,
                crn,
                companyDetailsConfirmed,
                chargeableForCT,
                ctIncomeDeclared,
                recentlyStartedTrading,
                ctutr
              )
            companySession.copy(userAnswers = completeAnswers)

          case individualSession @ IndividualHECSession(
                _,
                _,
                individualAnswers @ IncompleteIndividualUserAnswers(
                  Some(licenceType),
                  Some(licenceTimeTrading),
                  Some(licenceValidityPeriod),
                  Some(taxSituation),
                  saIncomeDeclared,
                  entityType
                ),
                _,
                _,
                _
              ) if allIndividualAnswersComplete(individualAnswers, individualSession) =>
            val completeAnswers =
              CompleteIndividualUserAnswers(
                licenceType,
                licenceTimeTrading,
                licenceValidityPeriod,
                taxSituation,
                saIncomeDeclared,
                entityType
              )
            individualSession.copy(userAnswers = completeAnswers)

          case _ => session
        }

        Right(updatedSession)
    }

  private def confirmIndividualDetailsRoute(session: HECSession): Call =
    if (session.unexpiredTaxChecks.nonEmpty) {
      routes.TaxChecksListController.unexpiredTaxChecks()
    } else {
      routes.LicenceDetailsController.licenceType()
    }

  private def licenceValidityPeriodRoute(session: HECSession): Call =
    session.userAnswers.fold(
      _.fold(_.licenceType, _.licenceType.some),
      _.fold(_.licenceType, _.licenceType.some)
    ) match {
      case Some(licenceType) =>
        if (licenceTypeForIndividualAndCompany(licenceType)) routes.EntityTypeController.entityType()
        else routes.TaxSituationController.taxSituation()
      case None              =>
        sys.error("Could not find licence type to work out route after licence validity period")
    }

  private def entityTypeRoute(session: HECSession): Call = {
    val maybeSelectedEntityType =
      session.userAnswers.fold(
        _.fold(_.entityType, _.entityType),
        _.fold(_.entityType, _.entityType.some)
      )
    val ggEntityType            = session.entityType

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
    session.mapAsIndividual { individualSession: IndividualHECSession =>
      individualSession.userAnswers.fold(_.taxSituation, _.taxSituation.some) match {
        case None =>
          sys.error("Could not find tax situation for tax situation route")

        case Some(taxSituation) =>
          if (saTaxSituations.contains(taxSituation)) {
            (individualSession.loginData.sautr, individualSession.retrievedJourneyData) match {
              case (Some(_), IndividualRetrievedJourneyData(Some(saStatus))) =>
                saStatus.status match {
                  case SAStatus.ReturnFound        => routes.SAController.saIncomeStatement()
                  case SAStatus.NoticeToFileIssued => routes.CheckYourAnswersController.checkYourAnswers()
                  case SAStatus.NoReturnFound      => routes.SAController.noReturnFound()
                }

              case (Some(_), IndividualRetrievedJourneyData(None)) =>
                sys.error("Found SA UTR for tax situation route but no SA status response")

              case (None, _) =>
                routes.SAController.sautrNotFound()
            }
          } else {
            routes.CheckYourAnswersController.checkYourAnswers()
          }
      }
    }

  private def companyRegistrationNumberRoute(session: HECSession) =
    session.mapAsCompany { companySession =>
      companySession.retrievedJourneyData.companyName.fold(
        sys.error("company name not found")
      )(_ => routes.CompanyDetailsController.confirmCompanyDetails())
    }

  private def confirmCompanyDetailsRoute(session: HECSession) =
    session.mapAsCompany { companySession =>
      companySession.userAnswers.fold(_.companyDetailsConfirmed, _.companyDetailsConfirmed.some) match {
        case Some(YesNoAnswer.Yes) =>
          (companySession.loginData.ctutr, companySession.retrievedJourneyData) match {
            // enrolment and DES CTUTR present but don't match
            case (Some(ctutr), CompanyRetrievedJourneyData(_, Some(desCtutr), _)) if ctutr =!= desCtutr =>
              routes.CompanyDetailsController.ctutrNotMatched()

            // enrolment and DES CTUTRs are present and match, CT status found
            case (Some(_), CompanyRetrievedJourneyData(_, Some(_), Some(ctStatus)))                     =>
              ctStatus.latestAccountingPeriod.map(_.ctStatus) match {
                case Some(_) => routes.CompanyDetailsController.chargeableForCorporationTax()
                case None    => routes.CompanyDetailsController.recentlyStartedTrading()
              }

            // enrolment and DES CTUTRs are present and match, but CT status couldn't be fetched
            case (Some(_), CompanyRetrievedJourneyData(_, Some(_), None))                               =>
              routes.CompanyDetailsController.cannotDoTaxCheck()

            // DES CTUTR not fetched
            case (_, CompanyRetrievedJourneyData(_, None, _))                                           =>
              routes.CompanyDetailsController.ctutrNotMatched()

            //enrollment CTUTR is not present
            case (None, _)                                                                              =>
              routes.CompanyDetailsController.enterCtutr()
          }
        case Some(YesNoAnswer.No)  => routes.CRNController.companyRegistrationNumber()
        case None                  => sys.error("Confirm company details answer was not found")
      }

    }

  private def chargeableForCTRoute(session: HECSession) =
    session.mapAsCompany { companySession =>
      companySession.userAnswers.fold(_.chargeableForCT, _.chargeableForCT) map {
        case YesNoAnswer.No  => routes.CheckYourAnswersController.checkYourAnswers()
        case YesNoAnswer.Yes =>
          companySession.retrievedJourneyData.ctStatus.flatMap(_.latestAccountingPeriod) match {
            case Some(accountingPeriod) =>
              accountingPeriod.ctStatus match {
                case CTStatus.ReturnFound        => routes.CompanyDetailsController.ctIncomeStatement()
                case CTStatus.NoticeToFileIssued => routes.CheckYourAnswersController.checkYourAnswers()
                case CTStatus.NoReturnFound      => routes.CompanyDetailsController.cannotDoTaxCheck()
              }
            case None                   => sys.error("CT status info missing")
          }
      } getOrElse {
        sys.error("Chargeable for CT answer missing")
      }
    }

  private def recentlyStartedTradingRoute(session: HECSession) =
    session.mapAsCompany { companySession =>
      companySession.userAnswers.fold(_.recentlyStartedTrading, _.recentlyStartedTrading) map {
        case YesNoAnswer.Yes => routes.CheckYourAnswersController.checkYourAnswers()
        case YesNoAnswer.No  => routes.CompanyDetailsController.cannotDoTaxCheck()
      } getOrElse {
        sys.error("Answer missing for if company has recently started trading")
      }
    }

  private def enterCtutrRoute(session: HECSession) =
    session.mapAsCompany { companySession =>
      val attemptsExceeded = companySession.ctutrAnswerAttempts >= appConfig.maxCtutrAnswerAttempts
      val ctutrOpt         = companySession.userAnswers.fold(_.ctutr, _.ctutr)
      if (attemptsExceeded) {
        routes.CompanyDetailsController.tooManyCtutrAttempts()
      } else {
        ctutrOpt map { _ =>
          companySession.retrievedJourneyData match {
            case CompanyRetrievedJourneyData(_, Some(_), Some(ctStatus)) =>
              ctStatus.latestAccountingPeriod.map(_.ctStatus) match {
                case Some(_) => routes.CompanyDetailsController.chargeableForCorporationTax()
                case None    => routes.CompanyDetailsController.recentlyStartedTrading()
              }
            case CompanyRetrievedJourneyData(_, Some(_), None)           =>
              routes.CompanyDetailsController.cannotDoTaxCheck()
            case _                                                       => sys.error("DES CTUTR missing in journey data")
          }
        } getOrElse {
          sys.error("CTUTR answer missing")
        }
      }
    }

}

object JourneyServiceImpl {
  def licenceTypeForIndividualAndCompany(licenceType: LicenceType): Boolean = licenceType match {
    case LicenceType.DriverOfTaxisAndPrivateHires => false
    case _                                        => true
  }

  /**
    * Expect the entity type to be specified when licence type is suitable for both individual and company
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
    saIncomeDeclared: Option[YesNoAnswer],
    retrievedJourneyData: IndividualRetrievedJourneyData
  ): Boolean =
    (retrievedJourneyData, saIncomeDeclared) match {
      case (IndividualRetrievedJourneyData(Some(SAStatusResponse(_, _, ReturnFound))), None)
          if saTaxSituations.contains(taxSituation) =>
        false
      case _ => true
    }

  private def checkCompanyDataComplete(
    chargeableForCTOpt: Option[YesNoAnswer],
    ctIncomeDeclaredOpt: Option[YesNoAnswer],
    recentlyStartedTradingOpt: Option[YesNoAnswer],
    companySession: CompanyHECSession
  ): Boolean =
    companySession.retrievedJourneyData.ctStatus match {
      case None                   => false
      case Some(ctStatusResponse) =>
        ctStatusResponse.latestAccountingPeriod.map(_.ctStatus) match {
          case None                              => recentlyStartedTradingOpt.contains(YesNoAnswer.Yes)
          case Some(CTStatus.NoticeToFileIssued) => chargeableForCTOpt.isDefined
          case Some(CTStatus.NoReturnFound)      => chargeableForCTOpt.contains(YesNoAnswer.No)
          case Some(CTStatus.ReturnFound)        =>
            chargeableForCTOpt.contains(YesNoAnswer.No) || (chargeableForCTOpt.contains(
              YesNoAnswer.Yes
            ) && ctIncomeDeclaredOpt.nonEmpty)
        }
    }

  /**
    * Process the incomplete individual answers and retrieved user data to determine if all answers have
    * been given by the user
    *
    * @param incompleteUserAnswers Individual incomplete answers
    * @param session Individual session data
    * @return A boolean representing whether or not the user has completed answering all relevant questions
    */
  def allIndividualAnswersComplete(
    incompleteUserAnswers: IncompleteIndividualUserAnswers,
    session: IndividualHECSession
  ): Boolean =
    incompleteUserAnswers match {
      case IncompleteIndividualUserAnswers(
            Some(licenceType),
            Some(_),
            Some(_),
            Some(taxSituation),
            saIncomeDeclared,
            entityType
          ) =>
        val licenceTypeCheck      = checkEntityTypePresentIfRequired(licenceType, entityType)
        val saIncomeDeclaredCheck =
          checkSAIncomeDeclared(taxSituation, saIncomeDeclared, session.retrievedJourneyData)
        licenceTypeCheck && saIncomeDeclaredCheck

      case _ => false
    }

  /**
    * Process the incomplete company answers and retrieved user data to determine if all answers have
    * been given by the user
    *
    * @param incompleteUserAnswers Company incomplete answers
    * @param session Company session data
    * @return A boolean representing whether or not the user has completed answering all relevant questions
    */
  def allCompanyAnswersComplete(
    incompleteUserAnswers: IncompleteCompanyUserAnswers,
    session: CompanyHECSession
  ): Boolean =
    incompleteUserAnswers match {
      case IncompleteCompanyUserAnswers(
            Some(_),
            Some(_),
            Some(_),
            Some(_),
            Some(_),
            Some(_),
            chargeableForCT,
            ctIncomeDeclared,
            recentlyStartedTrading,
            _
          ) =>
        checkCompanyDataComplete(chargeableForCT, ctIncomeDeclared, recentlyStartedTrading, session)

      case _ => false
    }
}
