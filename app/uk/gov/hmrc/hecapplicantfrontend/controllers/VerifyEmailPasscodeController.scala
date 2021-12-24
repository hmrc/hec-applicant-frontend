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

import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Passcode
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.verifyPasscodeForm
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.views.html.VerifyPasscode

import java.util.Locale
import scala.concurrent.ExecutionContext

class VerifyEmailPasscodeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  emailVerificationService: EmailVerificationService,
  verifyPasscodePage: VerifyPasscode,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val verifyEmailPasscode: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.ensureGGEmailIdPresent { ggEmail =>
      val passcodeOpt: Option[Passcode] =
        request.sessionData.fold(_.userEmailAnswers.flatMap(_.passcode), _.userEmailAnswers.flatMap(_.passcode))
      val form                          = passcodeOpt.fold(verifyPasscodeForm)(verifyPasscodeForm.fill)
      val back                          = journeyService.previous(routes.VerifyEmailPasscodeController.verifyEmailPasscode())

      Ok(verifyPasscodePage(form, back, ggEmail))
    }
  }

  val verifyEmailPasscodeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val session = request.sessionData
    session.ensureGGEmailIdPresent { ggEmail =>
      def handleValidPasscode(passcode: Passcode) = {
        val userSelectedEmail = session.userEmailAnswers.flatMap(_.emailAddress).getOrElse(ggEmail)
        val result            = for {
          passcodeVerificationResult <- emailVerificationService.verifyPasscode(passcode, userSelectedEmail)
          _                           = println("passcodeVerificationResult::  " + passcodeVerificationResult.toString)
          currentEmailAnswers         = session.userEmailAnswers
          updatedEmailAnswers         =
            currentEmailAnswers
              .map(_.copy(passcode = passcode.some, passcodeVerificationResult = passcodeVerificationResult.some))
          updatedSession              =
            session.fold(_.copy(userEmailAnswers = updatedEmailAnswers), _.copy(userEmailAnswers = updatedEmailAnswers))
          next                       <-
            journeyService.updateAndNext(routes.VerifyEmailPasscodeController.verifyEmailPasscode(), updatedSession)

        } yield next

        result.fold(
          _.doThrow("Could not update session and proceed"),
          Redirect
        )

      }
      verifyPasscodeForm()
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              verifyPasscodePage(
                formWithErrors,
                journeyService.previous(routes.VerifyEmailPasscodeController.verifyEmailPasscode()),
                ggEmail
              )
            ),
          handleValidPasscode
        )

    }
  }

}

object VerifyEmailPasscodeController {

  def verifyPasscodeForm(): Form[Passcode] = Form(
    mapping(
      "passcode" -> nonEmptyText
        .transform[Passcode](p => Passcode(p.toUpperCase(Locale.UK)), _.value)
    )(identity)(Some(_))
  )
}
