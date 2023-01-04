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
import cats.syntax.eq._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckCodesDisplayed
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailRequestedForTaxCheck, HECTaxCheckCode}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils._
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.ZonedDateTime
import java.util.Locale
import scala.concurrent.ExecutionContext

@Singleton
class TaxChecksListController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  auditService: AuditService,
  taxChecksListPage: html.TaxChecksList,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  implicit val dateOrdering: Ordering[ZonedDateTime] = (x: ZonedDateTime, y: ZonedDateTime) => y compareTo x

  implicit val licenceTypeOrdering: Ordering[LicenceType] =
    Ordering.fromLessThan { case (l1, l2) =>
      LicenceDetailsController.licenceTypes.indexOf(l1) < LicenceDetailsController.licenceTypes.indexOf(l2)
    }

  /** Fetches unexpired tax check codes for applicant
    */
  val unexpiredTaxChecks: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.unexpiredTaxChecks match {
      case Nil       =>
        InconsistentSessionState("No tax check codes found").doThrow
      case taxChecks =>
        auditService.sendEvent(
          TaxCheckCodesDisplayed(
            request.sessionData.loginData.ggCredId,
            taxChecks.map(_.taxCheckCode),
            request.language
          )
        )
        Ok(taxChecksListPage(taxChecks))
    }
  }

  val unexpiredTaxChecksSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    journeyService
      .updateAndNext(
        routes.TaxChecksListController.unexpiredTaxChecks,
        request.sessionData.fold(_.copy(emailRequestedForTaxCheck = None), _.copy(emailRequestedForTaxCheck = None))
      )
      .fold(
        _.doThrow("Could not save tax check"),
        Redirect
      )
  }

  def sendEmail(taxCheckCode: HECTaxCheckCode): Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.unexpiredTaxChecks match {
        case Nil       =>
          InconsistentSessionState("No tax check codes found").doThrow
        case taxChecks =>
          taxChecks.find(t => normalise(t.taxCheckCode) === normalise(taxCheckCode)) match {
            case None           =>
              sys.error(
                s"Received request to send tax check email for non-existent tax check code ${taxCheckCode.value}"
              )
            case Some(taxCheck) =>
              val emailRequestedForTaxCheck = EmailRequestedForTaxCheck(
                routes.TaxChecksListController.unexpiredTaxChecks.url,
                taxCheck
              )
              val updatedSession            = request.sessionData.fold(
                _.copy(emailRequestedForTaxCheck = Some(emailRequestedForTaxCheck)),
                _.copy(emailRequestedForTaxCheck = Some(emailRequestedForTaxCheck))
              )

              journeyService
                .updateAndNext(
                  routes.TaxChecksListController.unexpiredTaxChecks,
                  updatedSession
                )
                .fold(
                  _.doThrow("Could not update session and calculate next page"),
                  Redirect
                )
          }
      }
    }

  private def normalise(taxCheckCode: HECTaxCheckCode): HECTaxCheckCode =
    HECTaxCheckCode(taxCheckCode.value.toUpperCase(Locale.UK).removeWhitespace)

}
