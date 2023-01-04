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

import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.mapping
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.ConfirmEmailAddressController.differentEmailAddressMapping
import uk.gov.hmrc.hecapplicantfrontend.controllers.EnterEmailAddressController.enterEmailAddressForm
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailType, UserEmailAnswers, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

class EnterEmailAddressController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  emailVerificationService: EmailVerificationService,
  sessionStore: SessionStore,
  enterEmailAddressPage: html.EnterEmailaddress,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val enterEmailAddress: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.ensureEmailHasBeenRequested { _ =>
      val userEmailAnswerOpt: Option[UserEmailAnswers] = request.sessionData.userEmailAnswers

      val updatedSession =
        request.sessionData.fold(_.copy(hasResentEmailConfirmation = false), _.copy(hasResentEmailConfirmation = false))

      sessionStore
        .store(updatedSession)
        .fold(
          _.doThrow("Could not update session and proceed"),
          _ => {
            val req  = request.copy(sessionData = updatedSession)
            // reason for explicitly passing req and hc is same as mentioned in ConfirmEmailAddressController
            val back = journeyService.previous(routes.EnterEmailAddressController.enterEmailAddress)(req, hc)
            val form =
              userEmailAnswerOpt.fold(enterEmailAddressForm)(uea => enterEmailAddressForm.fill(uea.userSelectedEmail))
            Ok(enterEmailAddressPage(form, back))
          }
        )
    }
  }

  val enterEmailAddressSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.ensureEmailHasBeenRequested { _ =>
      def handleValidEmail(userSelectedEmail: UserSelectedEmail) = {
        val result = for {
          passcodeResult     <-
            emailVerificationService.requestPasscode(userSelectedEmail)
          updatedEmailAnswers =
            Some(UserEmailAnswers(userSelectedEmail, passcodeResult.some, None, None, None))
          updatedSession      =
            request.sessionData
              .fold(_.copy(userEmailAnswers = updatedEmailAnswers), _.copy(userEmailAnswers = updatedEmailAnswers))
          next               <-
            journeyService.updateAndNext(routes.EnterEmailAddressController.enterEmailAddress, updatedSession)
        } yield next

        result.fold(
          _.doThrow("Could not update session and proceed"),
          Redirect
        )

      }

      enterEmailAddressForm
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              enterEmailAddressPage(
                formWithErrors,
                journeyService.previous(routes.EnterEmailAddressController.enterEmailAddress)
              )
            ),
          handleValidEmail
        )
    }
  }

}

object EnterEmailAddressController {

  val enterEmailAddressForm: Form[UserSelectedEmail] = Form(
    mapping(
      "enterEmailAddress" -> differentEmailAddressMapping
    )(UserSelectedEmail(EmailType.DifferentEmail, _))(_.emailAddress.some)
  )
}
