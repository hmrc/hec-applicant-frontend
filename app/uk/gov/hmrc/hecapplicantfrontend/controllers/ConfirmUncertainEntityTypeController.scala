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
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, UncertainEntityTypeJourneyAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{EntityType, UncertainEntityTypeJourney}
import uk.gov.hmrc.hecapplicantfrontend.repos.{SessionStore, UncertainEntityTypeJourneyStore}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class ConfirmUncertainEntityTypeController @Inject() (
  authAction: AuthAction,
  uncertainEntityTypeJourneyAction: UncertainEntityTypeJourneyAction,
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

  private lazy val start: Call = routes.StartController.start

  private lazy val redirectToStart: Result = Redirect(start)

  val entityType: Action[AnyContent] = authAction.andThen(uncertainEntityTypeJourneyAction).async { implicit request =>
    val existingAnswer = request.sessionDataOrUncertainEntityTypeJourney.fold(
      _.entityType.some,
      _.userSuppliedEntityType
    )
    val emptyForm      = entityTypeForm(entityTypeOptions)
    val form           = existingAnswer.fold(emptyForm)(emptyForm.fill)

    Ok(entityTypePage(form, start, entityTypeOptions, routes.ConfirmUncertainEntityTypeController.entityTypeSubmit))
  }

  val entityTypeSubmit: Action[AnyContent] =
    authAction.andThen(uncertainEntityTypeJourneyAction).async { implicit request =>
      def handleValidEntityType(entityType: EntityType): Future[Result] =
        request.sessionDataOrUncertainEntityTypeJourney match {
          case Left(session) if session.entityType === entityType =>
            Redirect(journeyService.firstPage(session))

          case Right(journey) if journey.userSuppliedEntityType.contains(entityType) =>
            redirectToStart

          case originalState =>
            val result = for {
              _ <- originalState.fold(
                     _ => sessionStore.delete(),
                     _ => EitherT.pure(())
                   )
              _ <- uncertainEntityTypeJourneyStore.store(
                     UncertainEntityTypeJourney(
                       originalState.fold(_.loginData.ggCredId, _.ggCredId),
                       Some(entityType)
                     )
                   )
            } yield ()

            result.fold(_.doThrow("Could not perform update"), _ => redirectToStart)
        }

      entityTypeForm(entityTypeOptions)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              entityTypePage(
                formWithErrors,
                start,
                entityTypeOptions,
                routes.EntityTypeController.entityTypeSubmit
              )
            ),
          handleValidEntityType
        )
    }

}