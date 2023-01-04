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

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.verifyGGEmailInSession
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class CannotSendVerificationPasscodeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  cannotSendVerificationPasscodePage: html.CannotSendVerificationPasscoode,
  mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val cannotSendVerificationPasscode: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.userEmailAnswers.flatMap(_.passcodeRequestResult) match {
        case Some(PasscodeRequestResult.BadEmailAddress) =>
          val isGGEmailInSession = verifyGGEmailInSession(request.sessionData)
          val previous           =
            journeyService.previous(routes.CannotSendVerificationPasscodeController.cannotSendVerificationPasscode)
          Ok(cannotSendVerificationPasscodePage(isGGEmailInSession, previous))
        case other                                       =>
          InconsistentSessionState(
            s" Passcode request result found $other but the expected is Bad Email Request"
          ).doThrow

      }
    }

}
