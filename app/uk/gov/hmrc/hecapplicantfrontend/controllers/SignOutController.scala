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

import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html.TimedOut
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class SignOutController @Inject() (
  mcc: MessagesControllerComponents,
  timedOutPage: TimedOut,
  appConfig: AppConfig
) extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val signOutFromTimeout: Action[AnyContent] = Action { implicit request =>
    Ok(timedOutPage()).withNewSession
  }

  val exitSurvey: Action[AnyContent] = Action { _ =>
    Redirect(appConfig.exitSurveySignOutUrl)
  }
}
