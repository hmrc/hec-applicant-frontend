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

package uk.gov.hmrc.hecapplicantfrontend.filters

import akka.stream.Materializer
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.Results.Redirect
import play.api.mvc.{Filter, RequestHeader, Result}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import configs.syntax._

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

class EmailAllowedListFilter @Inject() (
  val mat: Materializer,
  val authConnector: AuthConnector,
  config: Configuration
)(implicit ec: ExecutionContext)
    extends Filter
    with AuthorisedFunctions
    with Logging {

  val userEmailListEnabled: Boolean               = config.underlying.getBoolean("email-allow-list.enabled")
  val userEmailAllowedListLowerCase: List[String] =
    config.underlying.get[List[String]]("email-allow-list.list").value.map(_.toLowerCase(Locale.UK))

  private def isExcludedEndpoint(rh: RequestHeader): Boolean =
    rh.path.contains(routes.AccessDeniedController.accessDenied().url) ||
      rh.path.contains("hmrc-frontend") ||
      rh.path.contains("assets") ||
      rh.path.contains("ping/ping")

  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] =
    if (userEmailListEnabled) {
      implicit val hc: HeaderCarrier =
        HeaderCarrierConverter.fromRequestAndSession(rh, rh.session)
      authorised()
        .retrieve(Retrievals.email) { emailOpt =>
          //if access denied point is in session uri or email list contains the email from the enrollment, call the function f
          //access denied uri is checked to avoid it to go to else part where it wil stuck in a loop
          val isAllowed = isExcludedEndpoint(rh) || emailOpt.exists(email =>
            userEmailAllowedListLowerCase.contains(email.toLowerCase(Locale.UK))
          )

          if (isAllowed) f(rh) else Future.successful(Redirect(routes.AccessDeniedController.accessDenied))
        }
        .recoverWith { case _: NoActiveSession => f(rh) }
    } else {
      f(rh)
    }
}
