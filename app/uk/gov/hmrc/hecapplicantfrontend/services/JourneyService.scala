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
import cats.instances.string._
import cats.instances.future._
import cats.syntax.eq._
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.mvc.Call
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models.{EntityType, Error, HECSession, LicenceType}
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils.LocalDateOps
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
    routes.LicenceDetailsController.licenceType()                        -> (_ => routes.LicenceDetailsController.expiryDate()),
    routes.LicenceDetailsController.expiryDate()                         -> licenceExpiryRoute,
    routes.LicenceDetailsController.licenceTimeTrading                   -> (_ => routes.LicenceDetailsController.recentLicenceLength()),
    routes.LicenceDetailsController.recentLicenceLength()                -> licenceValidityPeriodRoute,
    routes.EntityTypeController.entityType()                             -> entityTypeRoute,
    routes.TaxSituationController.taxSituation()                         -> (_ => routes.CheckYourAnswersController.checkYourAnswers())
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
      routes.LicenceDetailsController.expiryDateExit()  ->
        routes.LicenceDetailsController.expiryDate(),
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
    paths.get(current).map(_(updatedSession)) match {
      case None       =>
        EitherT.leftT(Error(s"Could not find next for $current"))
      case Some(next) =>
        if (r.sessionData === updatedSession)
          EitherT.pure(next)
        else
          sessionStore.store(updatedSession).map(_ => next)
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

    if (current === routes.StartController.start()) {
      current
    } else {
      exitPageToPreviousPage
        .get(current)
        .orElse(loop(routes.StartController.start()))
        .getOrElse(sys.error(s"Could not find previous for $current"))
    }
  }

  private def licenceValidityPeriodRoute(session: HECSession): Call =
    session.userAnswers.fold(_.licenceType, c => Some(c.licenceType)) match {
      case Some(LicenceType.DriverOfTaxisAndPrivateHires) => routes.TaxSituationController.taxSituation()
      case _                                              => routes.EntityTypeController.entityType()
    }

  private def licenceExpiryRoute(session: HECSession): Call =
    session.userAnswers.fold(_.licenceExpiryDate, c => Some(c.licenceExpiryDate)) match {
      case Some(expiryDate) if expiryDate.value.isAfterOrOn(TimeUtils.today().minusYears(1L)) =>
        routes.LicenceDetailsController.licenceTimeTrading()
      case _                                                                                  =>
        routes.LicenceDetailsController.expiryDateExit()
    }

  private def entityTypeRoute(session: HECSession): Call = {
    val selectedEntityType = session.userAnswers.fold(_.entityType, c => Some(c.entityType))
    val ggEntityType       = EntityType.fromRetrievedApplicantAnswers(session.retrievedUserData)

    if (selectedEntityType.contains(ggEntityType)) routes.TaxSituationController.taxSituation()
    else routes.EntityTypeController.wrongGGAccount()
  }

}
