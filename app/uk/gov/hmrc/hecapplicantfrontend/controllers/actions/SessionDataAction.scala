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

package uk.gov.hmrc.hecapplicantfrontend.controllers.actions

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.Results.Redirect
import play.api.mvc.{ActionRefiner, Result, WrappedRequest}
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.util.Logging

import scala.concurrent.{ExecutionContext, Future}

final case class RequestWithSessionData[A](
  request: AuthenticatedRequest[A],
  sessionData: HECSession
) extends WrappedRequest[A](request)

@Singleton
class SessionDataAction @Inject() (
  sessionStore: SessionStore
)(implicit val executionContext: ExecutionContext)
    extends ActionRefiner[AuthenticatedRequest, RequestWithSessionData]
    with Logging {

  override def refine[A](
    request: AuthenticatedRequest[A]
  ): Future[Either[Result, RequestWithSessionData[A]]] =
    sessionStore
      .get()(request)
      .leftMap(_.throws("Could not get session data"))
      .subflatMap(
        _.map(RequestWithSessionData(request, _))
          .toRight(Redirect(routes.StartController.start()))
      )
      .value

}
