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

import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class SAController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  sautrNotFoundPage: html.SautrNotFound,
  noReturnFoundPage: html.NoReturnFound
)(implicit appConfig: AppConfig)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  // Placeholder for confirm your income (HEC-985)
  val confirmYourIncome: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      s"Session is ${request.sessionData} back Url ::${journeyService.previous(routes.SAController.confirmYourIncome())}"
    )
  }

  val noReturnFound: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back = journeyService.previous(routes.SAController.noReturnFound())
    Ok(noReturnFoundPage(back))
  }

  val sautrNotFound: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back = journeyService.previous(routes.SAController.sautrNotFound())
    Ok(sautrNotFoundPage(back))
  }

}
