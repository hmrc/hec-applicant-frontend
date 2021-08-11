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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmIndividualDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  confirmIndividualDetailsPage: html.ConfirmIndividualDetails,
  confirmIndividualDetailsExitPage: html.ConfirmIndividualDetailsExit
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmIndividualDetails: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    withIndividualRetrievedData(request.sessionData) { i =>
      Ok(confirmIndividualDetailsPage(i))
    }
  }

  val confirmIndividualDetailsSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      withIndividualRetrievedData(request.sessionData) { _ =>
        journeyService
          .updateAndNext(routes.ConfirmIndividualDetailsController.confirmIndividualDetails(), request.sessionData)
          .fold(
            { e =>
              logger.warn("Could not update and find next page", e)
              InternalServerError
            },
            Redirect
          )
      }
    }

  val confirmIndividualDetailsExit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      withIndividualRetrievedData(request.sessionData) { _ =>
        val back = journeyService.previous(routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit())
        Ok(confirmIndividualDetailsExitPage(back))
      }
    }

  private def withIndividualRetrievedData(
    session: HECSession
  )(f: IndividualRetrievedData => Future[Result]): Future[Result] =
    session.retrievedUserData match {
      case i: IndividualRetrievedData => f(i)
      case _: CompanyRetrievedData    => Redirect(routes.StartController.start())
    }
}
