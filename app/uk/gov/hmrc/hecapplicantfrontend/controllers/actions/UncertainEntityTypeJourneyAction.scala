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

package uk.gov.hmrc.hecapplicantfrontend.controllers.actions

import cats.data.EitherT
import cats.syntax.either._
import com.google.inject.Inject
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionRefiner, Call, Result, WrappedRequest}
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession, Language, UncertainEntityTypeJourney}
import uk.gov.hmrc.hecapplicantfrontend.repos.{SessionStore, UncertainEntityTypeJourneyStore}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging

import scala.concurrent.{ExecutionContext, Future}

// Left(HECSession) indicates user has started the HEC service properly
// Right(UncertainEntityTypeJourney) indicates the user has not started the HEC service properly yet but the service has
// recognised a need for the user to clarify their entity type
final case class RequestWithUncertainEntityTypeJourney[A](
  request: AuthenticatedRequest[A],
  sessionDataOrUncertainEntityTypeJourney: Either[HECSession, UncertainEntityTypeJourney],
  language: Language
) extends WrappedRequest[A](request)

trait UncertainEntityTypeJourneyAction
    extends ActionRefiner[AuthenticatedRequest, RequestWithUncertainEntityTypeJourney]
    with Logging {

  // if true, session will not be returned on the left if didConfirmUncertainEntityType is not true
  // if false, session will be returned on the left even if  didConfirmUncertainEntityType is not defined or false
  val requireDidConfirmUncertainEntityType: Boolean

  val sessionStore: SessionStore

  val uncertainEntityTypeJourneyStore: UncertainEntityTypeJourneyStore

  implicit val executionContext: ExecutionContext

  private lazy val start: Call = routes.StartController.start

  private lazy val redirectToStart: Result = Redirect(start)

  override def refine[A](
    request: AuthenticatedRequest[A]
  ): Future[Either[Result, RequestWithUncertainEntityTypeJourney[A]]] = {
    lazy val language = Language.fromRequest(request.request).valueOr(sys.error)
    val result        = sessionStore
      .get()(request)
      .flatMap[Error, Either[Result, RequestWithUncertainEntityTypeJourney[A]]] {
        case Some(s)
            if !requireDidConfirmUncertainEntityType || s.loginData.didConfirmUncertainEntityType.contains(true) =>
          val r = RequestWithUncertainEntityTypeJourney(request, Left(s), language)
          EitherT.pure(Right(r))

        case Some(_) =>
          logger.warn("Existing session indicates user never confirmed an uncertain entity type. Redirecting to start")
          EitherT.pure(Left(redirectToStart))

        case None =>
          uncertainEntityTypeJourneyStore.get()(request).map {
            case Some(j) =>
              Right(RequestWithUncertainEntityTypeJourney(request, Right(j), language))

            case None =>
              logger.warn("Could not find session or UncertainEntityTypeJourney. Redirecting to start")
              Left(redirectToStart)
          }
      }

    result
      .leftMap(_.doThrow("Could not perform action"))
      .merge
  }
}

class RequireDidConfirmUncertainEntityTypeJourneyAction @Inject() (
  val sessionStore: SessionStore,
  val uncertainEntityTypeJourneyStore: UncertainEntityTypeJourneyStore
)(implicit val executionContext: ExecutionContext)
    extends UncertainEntityTypeJourneyAction {

  override val requireDidConfirmUncertainEntityType: Boolean = true

}

class NotRequireDidConfirmUncertainEntityTypeJourneyAction @Inject() (
  val sessionStore: SessionStore,
  val uncertainEntityTypeJourneyStore: UncertainEntityTypeJourneyStore
)(implicit val executionContext: ExecutionContext)
    extends UncertainEntityTypeJourneyAction {

  override val requireDidConfirmUncertainEntityType: Boolean = false

}
