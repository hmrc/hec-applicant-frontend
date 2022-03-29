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

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class TooManyEmailVerificationAttemptController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  mcc: MessagesControllerComponents,
  tooManyEmailVerificationAttemptsPage: html.TooManyEmailVerificationAttempts
) extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val tooManyEmailVerificationAttempts: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.userEmailAnswers.flatMap(_.passcodeRequestResult) match {
      case Some(PasscodeRequestResult.MaximumNumberOfEmailsExceeded) => Ok(tooManyEmailVerificationAttemptsPage())
      case other                                                     =>
        InconsistentSessionState(
          s"Passcode Request result found $other but the expected is Maximum Number Of Emails sExceeded"
        ).doThrow
    }

  }

}
