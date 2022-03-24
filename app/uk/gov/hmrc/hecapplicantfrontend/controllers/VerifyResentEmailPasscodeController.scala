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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.{getNextOrNoMatchResult, verifyGGEmailInSession, verifyPasscodeForm}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.Passcode
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

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
        val isGGEmailInSession  = verifyGGEmailInSession(session)
        val updatedEmailAddress = session.userEmailAnswers.map(_.copy(passcodeVerificationResult = None))
        val updatedSession      =
          request.sessionData.fold(
            _.copy(hasResentEmailConfirmation = true, userEmailAnswers = updatedEmailAddress),
            _.copy(hasResentEmailConfirmation = true, userEmailAnswers = updatedEmailAddress)
          )
        sessionStore
          .store(updatedSession)
          .fold(
            _.doThrow("Could not update session and proceed"),
            _ => {
              val req                           = request.copy(sessionData = updatedSession)
              val passcodeOpt: Option[Passcode] =
                session.fold(_.userEmailAnswers.flatMap(_.passcode), _.userEmailAnswers.flatMap(_.passcode))
              val form                          = passcodeOpt.fold(verifyPasscodeForm)(verifyPasscodeForm.fill)
              val back                          =
                journeyService.previous(routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode)(req, hc)
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
        val currentCall        = routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode

        def handleValidPasscode(passcode: Passcode): Future[Result] =
          getNextOrNoMatchResult(
            passcode,
            userSelectedEmail,
            currentCall,
            emailVerificationService,
            journeyService
          ).fold(
            _.doThrow("Could not update session and proceed"),
            _.fold(
              _ =>
                Ok(
                  verifyResentPasscodePage(
                    verifyPasscodeForm()
                      .bindFromRequest()
                      .withError("passcode", "error.noMatch"),
                    journeyService.previous(currentCall),
                    userSelectedEmail.emailAddress,
                    isGGEmailInSession
                  )
                ),
              Redirect
            )
          )

        verifyPasscodeForm()
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                verifyResentPasscodePage(
                  formWithErrors,
                  journeyService.previous(currentCall),
                  userSelectedEmail.emailAddress,
                  isGGEmailInSession
                )
              ),
            handleValidPasscode
          )
      }
    }

}
