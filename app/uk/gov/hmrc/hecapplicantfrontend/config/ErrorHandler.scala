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

package uk.gov.hmrc.hecapplicantfrontend.config

import javax.inject.{Inject, Singleton}
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Redirect
import play.api.mvc.{RequestHeader, Result}
import scala.concurrent.{ExecutionContext, Future}
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler
import uk.gov.hmrc.hecapplicantfrontend.views.html.ErrorTemplate

@Singleton
class ErrorHandler @Inject() (val messagesApi: MessagesApi, errorTemplate: ErrorTemplate)(implicit
  protected val ec: ExecutionContext
) extends FrontendErrorHandler
    with Logging {

  override def standardErrorTemplate(pageTitle: String, heading: String, message: String)(implicit
    request: RequestHeader
  ): Future[play.twirl.api.Html] =
    Future.successful(errorTemplate(pageTitle, heading, message))

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = exception match {
    case InconsistentSessionState(message) =>
      logger.warn(s"Found inconsistent session state for ${request.uri}: $message. Redirecting to the start endpoint.")
      Future.successful(Redirect(routes.StartController.start))

    case other =>
      super.onServerError(request, other)
  }

}
