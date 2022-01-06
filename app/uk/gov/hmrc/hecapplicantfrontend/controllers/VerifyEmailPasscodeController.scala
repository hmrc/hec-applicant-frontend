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

import cats.data.EitherT
import cats.implicits._
import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeVerificationResult}
import play.api.data.Form
import uk.gov.hmrc.hecapplicantfrontend.models._
import play.api.data.Forms.{mapping, nonEmptyText}
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.{verifyGGEmailInSession, verifyPasscodeForm}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.views.html.VerifyPasscode

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

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
    val session = request.sessionData
    session.ensureUserSelectedEmailPresent { userSelectedEmail =>
      val isGGEmailInSession            = verifyGGEmailInSession(session)
      val passcodeOpt: Option[Passcode] =
        session.fold(_.userEmailAnswers.flatMap(_.passcode), _.userEmailAnswers.flatMap(_.passcode))
      val form                          = passcodeOpt.fold(verifyPasscodeForm)(verifyPasscodeForm.fill)
      val back                          = journeyService.previous(routes.VerifyEmailPasscodeController.verifyEmailPasscode())
      Ok(verifyPasscodePage(form, back, userSelectedEmail.emailAddress, isGGEmailInSession))
    }

  }

  val verifyEmailPasscodeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val session = request.sessionData
    session.ensureUserSelectedEmailPresent { userSelectedEmail =>
      val isGGEmailInSession = verifyGGEmailInSession(session)

      def handleValidPasscode(passcode: Passcode): Future[Result] = {
        val result = for {
          passcodeVerificationResult <-
            emailVerificationService.verifyPasscode(passcode, userSelectedEmail.emailAddress)
          currentEmailAnswers         = session.userEmailAnswers
          updatedEmailAnswers         =
            currentEmailAnswers
              .map(_.copy(passcode = passcode.some, passcodeVerificationResult = passcodeVerificationResult.some))
          updatedSession              =
            session.fold(_.copy(userEmailAnswers = updatedEmailAnswers), _.copy(userEmailAnswers = updatedEmailAnswers))
          nextOrNoMatch              <- passcodeVerificationResult match {
                                          case PasscodeVerificationResult.NoMatch =>
                                            val noMatch
                                              : EitherT[Future, Error, Either[PasscodeVerificationResult.NoMatch.type, Call]] =
                                              EitherT.pure[Future, Error](Left(PasscodeVerificationResult.NoMatch))
                                            noMatch
                                          case _                                  =>
                                            val call: EitherT[Future, Error, Either[PasscodeVerificationResult.NoMatch.type, Call]] =
                                              journeyService
                                                .updateAndNext(
                                                  routes.VerifyEmailPasscodeController.verifyEmailPasscode(),
                                                  updatedSession
                                                )
                                                .map(Right(_))
                                            call
                                        }
        } yield nextOrNoMatch

        result.fold(
          _.doThrow("Could not update session and proceed"),
          _.fold(
            _ =>
              Ok(
                verifyPasscodePage(
                  verifyPasscodeForm()
                    .withError("passcode", "error.noMatch"),
                  journeyService.previous(routes.VerifyEmailPasscodeController.verifyEmailPasscode()),
                  userSelectedEmail.emailAddress,
                  isGGEmailInSession
                )
              ),
            Redirect
          )
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
                userSelectedEmail.emailAddress,
                isGGEmailInSession
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

  private def verifyGGEmailInSession(session: HECSession) =
    session.fold(_.loginData.emailAddress, _.loginData.emailAddress) match {
      case Some(email) if email.value.nonEmpty => true
      case _                                   => false
    }

}
