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

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthAction
import uk.gov.hmrc.hecapplicantfrontend.models.iv.IvErrorStatus.{PreconditionFailed => IvPreconditionFailed, _}
import uk.gov.hmrc.hecapplicantfrontend.services.IvService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.ExecutionContext

@Singleton
class IvFailureController @Inject() (
  authAction: AuthAction,
  ivService: IvService,
  mcc: MessagesControllerComponents,
  failedIvPage: views.html.iv.FailedIv,
  failedMatchingPage: views.html.iv.FailedMatching,
  insufficientEvidencePage: views.html.iv.InsufficientEvidence,
  lockedOutPage: views.html.iv.LockedOut,
  preconditionFailedPage: views.html.iv.PreconditionFailed,
  technicalIssuesPage: views.html.iv.TechnicalIssue,
  timeoutPage: views.html.iv.Timeout,
  userAbortedPage: views.html.iv.UserAborted
)(implicit ec: ExecutionContext, appConfig: AppConfig)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  def ivFailure(journeyId: UUID): Action[AnyContent] = authAction.async { implicit request =>
    ivService
      .getFailedJourneyStatus(journeyId)
      .fold(
        _.doThrow("Could not check IV journey error status"),
        { ivErrorStatus =>
          val redirectTo = ivErrorStatus match {
            case Incomplete           => routes.IvFailureController.technicalIssue
            case FailedMatching       => routes.IvFailureController.failedMatching
            case FailedIV             => routes.IvFailureController.failedIV
            case InsufficientEvidence => routes.IvFailureController.insufficientEvidence
            case LockedOut            => routes.IvFailureController.lockedOut
            case UserAborted          => routes.IvFailureController.userAborted
            case Timeout              => routes.IvFailureController.timedOut
            case TechnicalIssue       => routes.IvFailureController.technicalIssue
            case IvPreconditionFailed => routes.IvFailureController.preconditionFailed
            case Unknown(value)       =>
              logger.warn(s"Received unknown error response status from IV: $value")
              routes.IvFailureController.technicalIssue
          }
          Redirect(redirectTo)
        }
      )
  }

  val failedMatching: Action[AnyContent] =
    authAction(implicit r => Ok(failedMatchingPage()))

  val failedIV: Action[AnyContent] =
    authAction(implicit r => Ok(failedIvPage()))

  val insufficientEvidence: Action[AnyContent] =
    authAction(implicit r => Ok(insufficientEvidencePage()))

  val lockedOut: Action[AnyContent] =
    authAction(implicit r => Ok(lockedOutPage()))

  val userAborted: Action[AnyContent] =
    authAction(implicit r => Ok(userAbortedPage()))

  val timedOut: Action[AnyContent] =
    authAction(implicit r => Ok(timeoutPage()))

  val technicalIssue: Action[AnyContent] =
    authAction(implicit r => Ok(technicalIssuesPage()))

  val preconditionFailed: Action[AnyContent] =
    authAction(implicit r => Ok(preconditionFailedPage()))

  val retry: Action[AnyContent] =
    authAction(_ => Redirect(routes.StartController.start))

}
