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
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.{verifyGGEmailInSession, verifyPasscodeForm}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Passcode
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

class VerifyResentEmailPasscodeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  sessionStore: SessionStore,
  emailVerificationService: EmailVerificationService,
  verifyResentPasscodePage: html.VerifyResentPasscode,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val verifyResentEmailPasscode: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      val session = request.sessionData
      session.ensureUserSelectedEmailPresent { userSelectedEmail =>
        val updatedSession = request.sessionData.fold(
          //sets the resend flag to false to facilitate clicking on provide another email address.
          //since 'provide another email address' page is not a part of resend journey, we need to reset the flag to false
          //Set it to true on click of continue button
          _.copy(hasResentEmailConfirmation = false),
          _.copy(
            hasResentEmailConfirmation = false
          )
        )

        sessionStore
          .store(updatedSession)
          .fold(
            _.doThrow("Could not update session and proceed"),
            _ => {
              val isGGEmailInSession            = verifyGGEmailInSession(session)
              val passcodeOpt: Option[Passcode] =
                session.fold(_.userEmailAnswers.flatMap(_.passcode), _.userEmailAnswers.flatMap(_.passcode))
              val form                          = passcodeOpt.fold(verifyPasscodeForm)(verifyPasscodeForm.fill)
              val back                          = journeyService.previous(routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode())
              Ok(verifyResentPasscodePage(form, back, userSelectedEmail.emailAddress, isGGEmailInSession))
            }
          )

      }
    }

  val verifyResentEmailPasscodeSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      val session = request.sessionData
      session.ensureUserSelectedEmailPresent { userSelectedEmail =>
        val isGGEmailInSession = verifyGGEmailInSession(session)

        def handleValidPasscode(passcode: Passcode) = {
          val result = for {
            passcodeVerificationResult <-
              emailVerificationService.verifyPasscode(passcode, userSelectedEmail.emailAddress)
            currentEmailAnswers         = session.userEmailAnswers
            updatedEmailAnswers         =
              currentEmailAnswers
                .map(_.copy(passcode = passcode.some, passcodeVerificationResult = passcodeVerificationResult.some))
            updatedSession              =
              session
                .fold(
                  _.copy(userEmailAnswers = updatedEmailAnswers, hasResentEmailConfirmation = true),
                  _.copy(userEmailAnswers = updatedEmailAnswers, hasResentEmailConfirmation = true)
                )
            next                       <-
              journeyService
                .updateAndNext(routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode(), updatedSession)

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
                verifyResentPasscodePage(
                  formWithErrors,
                  journeyService.previous(routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode()),
                  userSelectedEmail.emailAddress,
                  isGGEmailInSession
                )
              ),
            handleValidPasscode
          )
      }
    }

}
