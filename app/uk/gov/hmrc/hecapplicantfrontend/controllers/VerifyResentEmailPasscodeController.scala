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

//import cats.data.EitherT
//import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.{getNextOrNoMatchResult, verifyGGEmailInSession, verifyPasscodeForm}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
//import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode}
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

class VerifyResentEmailPasscodeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
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
        val isGGEmailInSession            = verifyGGEmailInSession(session)
        val passcodeOpt: Option[Passcode] =
          session.fold(_.userEmailAnswers.flatMap(_.passcode), _.userEmailAnswers.flatMap(_.passcode))
        val form                          = passcodeOpt.fold(verifyPasscodeForm)(verifyPasscodeForm.fill)
        val back                          = journeyService.previous(routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode())
        Ok(verifyResentPasscodePage(form, back, userSelectedEmail.emailAddress, isGGEmailInSession))

      }
    }

  val verifyResentEmailPasscodeSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      val session = request.sessionData
      session.ensureUserSelectedEmailPresent { userSelectedEmail =>
        val isGGEmailInSession = verifyGGEmailInSession(session)
        val currentCall        = routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode()

//        def getNextOrNoMatch(passcodeVerificationResult: PasscodeVerificationResult, updatedSession: HECSession)
//          : EitherT[Future, Error, Either[PasscodeVerificationResult, Call]] = passcodeVerificationResult match {
//
//          case PasscodeVerificationResult.NoMatch =>
//            EitherT.pure[Future, Error](Left(PasscodeVerificationResult.NoMatch))
//          case _                                  =>
//            journeyService
//              .updateAndNext(routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode(), updatedSession)
//              .map(Right(_))
//        }

        def handleValidPasscode(passcode: Passcode): Future[Result] =
//          val result = for {
//            passcodeVerificationResult <-
//              emailVerificationService.verifyPasscode(passcode, userSelectedEmail.emailAddress)
//            currentEmailAnswers         = session.userEmailAnswers
//            updatedEmailAnswers         =
//              currentEmailAnswers
//                .map(_.copy(passcode = passcode.some, passcodeVerificationResult = passcodeVerificationResult.some))
//            updatedSession              =
//              session
//                .fold(
//                  _.copy(userEmailAnswers = updatedEmailAnswers),
//                  _.copy(userEmailAnswers = updatedEmailAnswers)
//                )
//
//            nextOrNoMatch <- getNextOrNoMatch(passcodeVerificationResult, updatedSession)
//
//          } yield nextOrNoMatch
          getNextOrNoMatchResult(
            passcode,
            userSelectedEmail.emailAddress,
            currentCall,
            emailVerificationService,
            journeyService,
            false
          ).fold(
            _.doThrow("Could not update session and proceed"),
            _.fold(
              _ =>
                Ok(
                  verifyResentPasscodePage(
                    verifyPasscodeForm()
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
