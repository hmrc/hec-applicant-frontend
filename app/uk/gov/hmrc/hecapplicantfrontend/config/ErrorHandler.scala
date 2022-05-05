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

package uk.gov.hmrc.hecapplicantfrontend.config

import javax.inject.{Inject, Singleton}
import play.api.i18n.MessagesApi
import play.api.mvc.Results.Redirect
import play.api.mvc.{Request, RequestHeader, Result}
import play.twirl.api.Html
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.http.FrontendErrorHandler
import uk.gov.hmrc.hecapplicantfrontend.views.html.ErrorTemplate

import scala.concurrent.Future

@Singleton
class ErrorHandler @Inject() (errorTemplate: ErrorTemplate, val messagesApi: MessagesApi)
    extends FrontendErrorHandler
    with Logging {

  override def standardErrorTemplate(pageTitle: String, heading: String, message: String)(implicit
    request: Request[_]
  ): Html =
    errorTemplate(pageTitle, heading, message)

  override def onServerError(request: RequestHeader, exception: Throwable): Future[Result] = exception match {
    case InconsistentSessionState(message) =>
      logger.warn(s"Found inconsistent session state, redirecting to the start endpoint: $message")
      Future.successful(Redirect(routes.StartController.start))

    case other =>
      super.onServerError(request, other)
  }

}
