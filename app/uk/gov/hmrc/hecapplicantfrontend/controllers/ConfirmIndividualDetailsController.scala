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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.instances.future.*
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class ConfirmIndividualDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  confirmIndividualDetailsPage: html.ConfirmIndividualDetails,
  confirmIndividualDetailsExitPage: html.ConfirmIndividualDetailsExit,
  cannotFindDetailsPage: html.CannotFindIndividualDetails
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmIndividualDetails: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.mapAsIndividual { i =>
      val back = journeyService.previous(routes.ConfirmIndividualDetailsController.confirmIndividualDetails)
      Ok(confirmIndividualDetailsPage(back, i.loginData))
    }
  }

  val confirmIndividualDetailsSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.mapAsIndividual { individualSession =>
        journeyService
          .updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails,
            individualSession.copy(hasConfirmedDetails = true)
          )
          .fold(
            _.doThrow("Could not update and find next page"),
            Redirect
          )
      }
    }

  val confirmIndividualDetailsExit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.mapAsIndividual { _ =>
        val back = journeyService.previous(routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit)
        Ok(confirmIndividualDetailsExitPage(back))
      }
    }

  val cannotFindDetails: Action[AnyContent] =
    authAction { implicit request =>
      Ok(cannotFindDetailsPage())
    }

}
