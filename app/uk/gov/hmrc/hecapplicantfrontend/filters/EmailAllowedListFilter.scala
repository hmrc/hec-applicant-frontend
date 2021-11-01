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

package uk.gov.hmrc.hecapplicantfrontend.filters

import akka.stream.Materializer
import com.google.inject.Inject
import play.api.Configuration
import play.api.mvc.Results.{Redirect}
import play.api.mvc.{Filter, RequestHeader, Result}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.http.HeaderCarrierConverter

import java.util
import scala.concurrent.{ExecutionContext, Future}

class EmailAllowedListFilter @Inject() (
  val mat: Materializer,
  val authConnector: AuthConnector,
  config: Configuration
)(implicit ec: ExecutionContext)
    extends Filter
    with AuthorisedFunctions
    with Logging {

  val userEmailListEnabled: Boolean           = config.underlying.getBoolean("userAllowedList.enabled")
  val userEmailAllowedList: util.List[String] = config.underlying.getStringList("user-allow-list")

  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] =
    if (userEmailListEnabled) {
      println(" inside checking user email list")
      implicit val hc: HeaderCarrier =
        HeaderCarrierConverter.fromRequestAndSession(rh, rh.session)
      authorised()
        .retrieve(Retrievals.email) { email =>
          if (email.exists(x => userEmailAllowedList.contains(x))) {
            println(" inside first if")
            f(rh)
          } else {
            println(" inside first else")
            Future.successful(Redirect(routes.AccessDeniedController.accessDenied))
          }
        }
        .recoverWith { case _: NoActiveSession =>
          println(" inside case")
          f(rh)
        }
    } else {
      f(rh)
    }

//  override def apply(f: RequestHeader => Future[Result])(rh: RequestHeader): Future[Result] =
//    if (userEmailListEnabled) {
//      println(" inside checking user email list")
//      implicit val hc: HeaderCarrier =
//        HeaderCarrierConverter.fromRequestAndSession(rh, rh.session)
//      authConnector
//        .authorise(AuthProviders(AuthProvider.GovernmentGateway), Retrievals.email)
//        .flatMap { x =>
//          println(" inside flatmap::" + x.toString)
//          x match {
//            case Some(email) =>
//              println(" email is ::" + email)
//              if (userEmailAllowedList.contains(email)) {
//                f(rh)
//              } else { Future.successful(Redirect(routes.AccessDeniedController.accessDenied)) }
//            case None        =>
//              println(" inside None")
//              Future.successful(Redirect(routes.AccessDeniedController.accessDenied))
//          }
//
//        }
//        .recoverWith { case _: NoActiveSession => f(rh) }
//    } else {
//      f(rh)
//    }
}
