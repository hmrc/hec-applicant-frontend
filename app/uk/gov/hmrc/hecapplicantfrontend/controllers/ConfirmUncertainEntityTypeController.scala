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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import cats.syntax.option._
import cats.syntax.eq._
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequireDidConfirmUncertainEntityTypeJourneyAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{CompleteUserAnswers, EntityType, HECSession, IncompleteUserAnswers, UncertainEntityTypeJourney}
import uk.gov.hmrc.hecapplicantfrontend.repos.{SessionStore, UncertainEntityTypeJourneyStore}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class ConfirmUncertainEntityTypeController @Inject() (
  authAction: AuthAction,
  uncertainEntityTypeJourneyAction: RequireDidConfirmUncertainEntityTypeJourneyAction,
  uncertainEntityTypeJourneyStore: UncertainEntityTypeJourneyStore,
  journeyService: JourneyService,
  sessionStore: SessionStore,
  entityTypePage: html.EntityType,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  import EntityTypeController._

  private def start(isScotNIPrivateBeta: Option[Boolean]): Call =
    if (isScotNIPrivateBeta.contains(true)) routes.StartController.scotNIPrivateBetaStart
    else routes.StartController.start

  private def redirectToStart(isScotNIPrivateBeta: Option[Boolean]): Result =
    Redirect(start(isScotNIPrivateBeta))

  private def calculateBack(
    sessionDataOrUncertainEntityTypeJourney: Either[HECSession, UncertainEntityTypeJourney]
  ): Call =
    if (isSessionWithCompleteAnswers(sessionDataOrUncertainEntityTypeJourney))
      routes.CheckYourAnswersController.checkYourAnswers
    else
      start(sessionDataOrUncertainEntityTypeJourney.fold(_.isScotNIPrivateBeta, _.isScotNIPrivateBeta))

  private def isSessionWithCompleteAnswers(
    sessionDataOrUncertainEntityTypeJourney: Either[HECSession, UncertainEntityTypeJourney]
  ) = sessionDataOrUncertainEntityTypeJourney match {
    case Left(s: HECSession) =>
      s.userAnswers match {
        case _: CompleteUserAnswers   => true
        case _: IncompleteUserAnswers => false
      }

    case Right(_: UncertainEntityTypeJourney) =>
      false
  }

  val entityType: Action[AnyContent] = authAction.andThen(uncertainEntityTypeJourneyAction).async { implicit request =>
    val existingAnswer = request.sessionDataOrUncertainEntityTypeJourney.fold(
      _.entityType.some,
      _.userSuppliedEntityType
    )
    val emptyForm      = entityTypeForm(entityTypeOptions)
    val form           = existingAnswer.fold(emptyForm)(emptyForm.fill)
    val back           = calculateBack(request.sessionDataOrUncertainEntityTypeJourney)

    Ok(entityTypePage(form, back, entityTypeOptions, routes.ConfirmUncertainEntityTypeController.entityTypeSubmit))
  }

  val entityTypeSubmit: Action[AnyContent] =
    authAction.andThen(uncertainEntityTypeJourneyAction).async { implicit request =>
      def handleValidEntityType(entityType: EntityType): Future[Result] =
        request.sessionDataOrUncertainEntityTypeJourney match {
          case Left(session) if session.entityType === entityType =>
            val redirectTo =
              if (isSessionWithCompleteAnswers(request.sessionDataOrUncertainEntityTypeJourney))
                routes.CheckYourAnswersController.checkYourAnswers
              else
                journeyService.firstPage(session)
            Redirect(redirectTo)

          case Right(journey) if journey.userSuppliedEntityType.contains(entityType) =>
            redirectToStart(journey.isScotNIPrivateBeta)

          case originalState =>
            val isScotNIPrivateBeta = originalState.fold(_.isScotNIPrivateBeta, _.isScotNIPrivateBeta)
            val result              = for {
              _ <- originalState.fold(
                     _ => sessionStore.delete(),
                     _ => EitherT.pure(())
                   )
              _ <- uncertainEntityTypeJourneyStore.store(
                     UncertainEntityTypeJourney(
                       originalState.fold(_.loginData.ggCredId, _.ggCredId),
                       Some(entityType),
                       isScotNIPrivateBeta,
                       originalState.fold(_.isScotNIPrivateBetaEngWalUser, _.isScotNIPrivateBetaEngWalUser)
                     )
                   )
            } yield ()

            result.fold(_.doThrow("Could not perform update"), _ => redirectToStart(isScotNIPrivateBeta))
        }

      entityTypeForm(entityTypeOptions)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              entityTypePage(
                formWithErrors,
                calculateBack(request.sessionDataOrUncertainEntityTypeJourney),
                entityTypeOptions,
                routes.EntityTypeController.entityTypeSubmit
              )
            ),
          handleValidEntityType
        )
    }

}
