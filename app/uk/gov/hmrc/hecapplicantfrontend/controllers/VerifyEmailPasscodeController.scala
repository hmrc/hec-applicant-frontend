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

import cats.data.EitherT
import cats.implicits._
import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, nonEmptyText}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.controllers.VerifyEmailPasscodeController.{getNextOrNoMatchResult, verifyGGEmailInSession, verifyPasscodeForm}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.{HECSession, _}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.ControllerUtils.noXssChars
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils._
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

class VerifyEmailPasscodeController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  emailVerificationService: EmailVerificationService,
  sessionStore: SessionStore,
  verifyPasscodePage: html.VerifyPasscode,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val verifyEmailPasscode: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val session = request.sessionData
    session.ensureUserSelectedEmailPresent { userSelectedEmail =>
      val isGGEmailInSession  = verifyGGEmailInSession(session)
      val updatedEmailAddress = session.userEmailAnswers.map(_.copy(passcodeVerificationResult = None))
      val updatedSession      =
        request.sessionData.fold(
          _.copy(hasResentEmailConfirmation = false, userEmailAnswers = updatedEmailAddress),
          _.copy(hasResentEmailConfirmation = false, userEmailAnswers = updatedEmailAddress)
        )

      // update the hasResentEmailConfirmation here cause the flag set at Resend confirmation flag was not working as expected.

      sessionStore
        .store(updatedSession)
        .fold(
          _.doThrow("Could not update session and proceed"),
          _ => {
            val req                           = request.copy(sessionData = updatedSession)
            val passcodeOpt: Option[Passcode] =
              session.fold(_.userEmailAnswers.flatMap(_.passcode), _.userEmailAnswers.flatMap(_.passcode))
            val form                          = passcodeOpt.fold(verifyPasscodeForm())(verifyPasscodeForm().fill)
            val back                          = journeyService.previous(routes.VerifyEmailPasscodeController.verifyEmailPasscode)(req, hc)
            Ok(verifyPasscodePage(form, back, userSelectedEmail.emailAddress, isGGEmailInSession))
          }
        )

    }

  }

  val verifyEmailPasscodeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val session = request.sessionData
    session.ensureUserSelectedEmailPresent { userSelectedEmail =>
      val isGGEmailInSession = verifyGGEmailInSession(session)
      val currentCall        = routes.VerifyEmailPasscodeController.verifyEmailPasscode

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
                verifyPasscodePage(
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
              verifyPasscodePage(
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

object VerifyEmailPasscodeController {

  private val upperCaseVowels: List[Char] = List('A', 'E', 'I', 'O', 'U')

  def verifyPasscodeForm(): Form[Passcode] = Form(
    mapping(
      "passcode" -> nonEmptyText
        .verifying(noXssChars("error.format"))
        .transform[Passcode](p => Passcode(p.removeWhitespace.toUpperCase(Locale.UK)), _.value)
        .verifying("error.format", p => p.value.length === 6 && !p.value.exists(upperCaseVowels.contains(_)))
    )(identity)(Some(_))
  )

  def verifyGGEmailInSession(session: HECSession): Boolean =
    session.fold(_.loginData.emailAddress, _.loginData.emailAddress) match {
      case Some(email) if email.value.nonEmpty => true
      case _                                   => false
    }

  def getNextOrNoMatch(
    passcodeVerificationResult: PasscodeVerificationResult,
    updatedSession: HECSession,
    journeyService: JourneyService,
    currentCall: Call
  )(implicit
    ec: ExecutionContext,
    request: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): EitherT[Future, Error, Either[PasscodeVerificationResult, Call]] =
    passcodeVerificationResult match {

      case PasscodeVerificationResult.NoMatch => EitherT.pure[Future, Error](Left(PasscodeVerificationResult.NoMatch))
      case _                                  =>
        journeyService
          .updateAndNext(
            currentCall,
            updatedSession
          )
          .map(Right(_))
    }

  def getNextOrNoMatchResult(
    passcode: Passcode,
    userSelectedEmail: UserSelectedEmail,
    currentCall: Call,
    emailVerificationService: EmailVerificationService,
    journeyService: JourneyService
  )(implicit
    hc: HeaderCarrier,
    ec: ExecutionContext,
    request: RequestWithSessionData[_]
  ): EitherT[Future, Error, Either[PasscodeVerificationResult, Call]] = for {
    passcodeVerificationResult <-
      emailVerificationService.verifyPasscode(passcode, userSelectedEmail)
    currentEmailAnswers         = request.sessionData.userEmailAnswers
    updatedEmailAnswers         =
      currentEmailAnswers
        .map(_.copy(passcode = passcode.some, passcodeVerificationResult = passcodeVerificationResult.some))
    updatedSession              = getUpdatedSession(request.sessionData, updatedEmailAnswers)
    nextOrNoMatch              <- getNextOrNoMatch(passcodeVerificationResult, updatedSession, journeyService, currentCall)
  } yield nextOrNoMatch

  private def getUpdatedSession(session: HECSession, updatedEmailAnswers: Option[UserEmailAnswers]) =
    session
      .fold(
        _.copy(userEmailAnswers = updatedEmailAnswers),
        _.copy(userEmailAnswers = updatedEmailAnswers)
      )
}
