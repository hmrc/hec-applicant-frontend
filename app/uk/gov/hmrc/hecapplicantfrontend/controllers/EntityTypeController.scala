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

import cats.instances.future._
import cats.syntax.option._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class EntityTypeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  entityTypePage: html.EntityType,
  wrongGGAccountPage: html.WrongGGAccount,
  wrongEntityTypePage: html.WrongEntityType
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  import EntityTypeController._

  val entityType: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back       = journeyService.previous(routes.EntityTypeController.entityType)
    val entityType = request.sessionData.userAnswers.fold(
      _.fold(_.entityType, _.entityType),
      _.fold(_.entityType, _.entityType.some)
    )
    val form = {
      val emptyForm = entityTypeForm(entityTypeOptions)
      entityType.fold(emptyForm)(emptyForm.fill)
    }

    Ok(entityTypePage(form, back, entityTypeOptions))
  }

  val entityTypeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleValidEntityType(entityType: EntityType): Future[Result] = {
      val updatedSession = request.sessionData.replaceField(
        request.sessionData,
        IncompleteIndividualUserAnswers.entityType,
        IncompleteCompanyUserAnswers.entityType,
        _.copy(entityType = Some(entityType)),
        _.copy(entityType = Some(entityType))
      )
      journeyService
        .updateAndNext(
          routes.EntityTypeController.entityType,
          updatedSession
        )
        .fold(
          _.doThrow("Could not update session and proceed"),
          Redirect
        )
    }

    entityTypeForm(entityTypeOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            entityTypePage(
              formWithErrors,
              journeyService.previous(routes.EntityTypeController.entityType),
              entityTypeOptions
            )
          ),
        handleValidEntityType
      )
  }

  val wrongGGAccount: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.userAnswers.fold(
      _.fold(_.entityType, _.entityType),
      _.fold(_.entityType, _.entityType.some)
    ) match {
      case Some(entityType) =>
        val back = journeyService.previous(routes.EntityTypeController.wrongGGAccount)
        Ok(wrongGGAccountPage(back, entityType))

      case None =>
        InconsistentSessionState("Could not find entity type").doThrow
    }

  }

  val wrongEntityType: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.EntityTypeController.wrongEntityType)
    Ok(wrongEntityTypePage(back))
  }

}

object EntityTypeController {

  val entityTypeOptions: List[EntityType] = List(EntityType.Individual, EntityType.Company)

  def entityTypeForm(options: List[EntityType]): Form[EntityType] =
    Form(
      mapping(
        "entityType" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

}
