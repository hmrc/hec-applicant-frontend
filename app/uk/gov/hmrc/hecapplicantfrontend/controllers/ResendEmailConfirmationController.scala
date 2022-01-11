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

import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.views.html

import scala.concurrent.ExecutionContext

class ResendEmailConfirmationController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  emailVerificationService: EmailVerificationService,
  resendEmailConfirmationPage: html.ResendEmailConfirmationPage,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val resendEmail: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.ensureUserSelectedEmailPresent { userSelectedEmail =>
        val back = journeyService.previous(routes.ResendEmailConfirmationController.resendEmail())
        Ok(resendEmailConfirmationPage(userSelectedEmail.emailAddress, back))
      }
    }

  val resendEmailSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.ensureUserSelectedEmailPresent { userSelectedEmail =>
        val result = for {
          passcodeResult      <- emailVerificationService.requestPasscode(userSelectedEmail.emailAddress)
          existingEmailAnswers = request.sessionData.userEmailAnswers
          updatedEmailAnswers  = existingEmailAnswers.map(_.copy(passcodeRequestResult = passcodeResult.some))
          updatedSession       =
            request.sessionData
              .fold(
                //Sets the resend flag true as soon as Resend button is clicked
                _.copy(userEmailAnswers = updatedEmailAnswers, hasResentEmailConfirmation = true),
                _.copy(userEmailAnswers = updatedEmailAnswers, hasResentEmailConfirmation = true)
              )
          next                <- journeyService.updateAndNext(routes.ResendEmailConfirmationController.resendEmail(), updatedSession)
        } yield next
        result.fold(
          _.doThrow("Could not update session and proceed"),
          Redirect
        )
      }

    }

}
