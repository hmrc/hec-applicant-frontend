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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.ZonedDateTime
import scala.concurrent.ExecutionContext

@Singleton
class TaxChecksListController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  taxChecksListPage: html.TaxChecksList,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  implicit val dateOrdering: Ordering[ZonedDateTime] = (x: ZonedDateTime, y: ZonedDateTime) => y compareTo x

  /**
    * Fetches unexpired tax check codes for applicant
    */
  val unexpiredTaxChecks: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.unexpiredTaxChecks match {
      case Nil       =>
        sys.error("No tax check codes found")
      case taxChecks =>
        val sorted = taxChecks.sortBy(_.createDate)
        val back   = journeyService.previous(routes.TaxChecksListController.unexpiredTaxChecks())
        Ok(taxChecksListPage(back, sorted))
    }
  }

  val unexpiredTaxChecksSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    journeyService
      .updateAndNext(routes.TaxChecksListController.unexpiredTaxChecks(), request.sessionData)
      .fold(
        _.throws("Could not save tax check"),
        Redirect
      )
  }
}
