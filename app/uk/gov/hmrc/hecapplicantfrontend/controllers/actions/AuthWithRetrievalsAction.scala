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

package uk.gov.hmrc.hecapplicantfrontend.controllers.actions

import cats.syntax.eq.*
import com.google.inject.{Inject, Singleton}
import play.api.mvc.Results.Redirect
import play.api.mvc.*
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.~
import uk.gov.hmrc.auth.core.{AuthConnector, AuthorisationException, AuthorisedFunctions, NoActiveSession}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed
import uk.gov.hmrc.hecapplicantfrontend.models.{AuthenticationStatus, RetrievedGGData}
import uk.gov.hmrc.hecapplicantfrontend.services.AuditService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedRequestWithRetrievedGGData[A](
  retrievedGGUserData: RetrievedGGData,
  request: Request[A]
) extends WrappedRequest[A](request)

@Singleton
class AuthWithRetrievalsAction @Inject() (
  val authConnector: AuthConnector,
  auditService: AuditService,
  mcc: MessagesControllerComponents,
  appConfig: AppConfig
)(implicit val executionContext: ExecutionContext)
    extends ActionFunction[Request, AuthenticatedRequestWithRetrievedGGData]
    with ActionBuilder[AuthenticatedRequestWithRetrievedGGData, AnyContent]
    with AuthorisedFunctions
    with Logging {

  override def invokeBlock[A](
    request: Request[A],
    block: AuthenticatedRequestWithRetrievedGGData[A] => Future[Result]
  ): Future[Result] = {
    implicit val hc: HeaderCarrier =
      HeaderCarrierConverter.fromRequestAndSession(request, request.session)

    authorised()
      .retrieve(
        Retrievals.confidenceLevel and
          Retrievals.affinityGroup and
          Retrievals.nino and
          Retrievals.saUtr and
          Retrievals.email and
          Retrievals.allEnrolments and
          Retrievals.credentials
      ) { case cl ~ affinityGroup ~ maybeNino ~ maybeSautr ~ maybeEmail ~ enrolments ~ creds =>
        block(
          AuthenticatedRequestWithRetrievedGGData(
            RetrievedGGData(
              cl,
              affinityGroup,
              maybeNino,
              maybeSautr,
              maybeEmail,
              enrolments,
              creds
            ),
            request
          )
        )
      }
      .recover {
        case _: NoActiveSession =>
          val redirectTo = appConfig.signInUrl(routes.StartController.start)
          auditService.sendEvent(
            ApplicantServiceStartEndPointAccessed(
              AuthenticationStatus.NotAuthenticated,
              Some(redirectTo.takeWhile(_ =!= '?')),
              None
            )
          )(hc, ApplicantServiceStartEndPointAccessed.writes, request)
          Redirect(redirectTo)

        case e: AuthorisationException => sys.error(s"Could not authorise: ${e.getMessage}")

      }
  }

  override def parser: BodyParser[AnyContent] = mcc.parsers.defaultBodyParser
}
