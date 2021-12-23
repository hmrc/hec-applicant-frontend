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

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Passcode
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.verifyPasscodeForm
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, UserEmailAnswers}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.views.html.VerifyPasscode

class VerifyEmailPasscodeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  verifyPasscodePage: VerifyPasscode,
  mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val verifyEmailPasscode: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val userEmailAnswerOpt: Option[UserEmailAnswers]                 = request.sessionData.userEmailAnswers
    val (passcode: Option[Passcode], emailOpt: Option[EmailAddress]) = userEmailAnswerOpt match {
      case Some(answers) => (answers.passcode, answers.emailAddress)
      case _             => (None, None)
    }
    val email: EmailAddress                                          = emailOpt match {
      case Some(e) => e
      case _       =>
        request.sessionData
          .fold(_.loginData.emailAddress, _.loginData.emailAddress)
          .getOrElse(sys.error("No  Email Address found in GG account"))
    }
    val form                                                         = passcode.fold(verifyPasscodeForm())(verifyPasscodeForm().fill(_))
    val back                                                         = journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress())

    Ok(verifyPasscodePage(form, back, email))
  }

  val verifyEmailPasscodeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(s"${request.sessionData}")
  }

}

object VerifyEmailPasscodeController {
  def verifyPasscodeForm(): Form[Passcode] = Form(
    mapping(
      "passcode" -> nonEmptyText
        .transform[Passcode](f => Passcode(f), _.value)
    )(identity)(Some(_))
  )
}
