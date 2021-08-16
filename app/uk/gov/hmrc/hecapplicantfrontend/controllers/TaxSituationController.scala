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
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.{reportEarnedIncomeOptions, reportIncomeForm}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.ReportIncomeEarned._
import uk.gov.hmrc.hecapplicantfrontend.models.{LicenceType, ReportIncomeEarned}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaxSituationController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  reportIncomeEarnedPage: html.ReportIncomeEarned
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val taxSituation: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val licenceTypeOpt = request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType))
    licenceTypeOpt match {
      case Some(licenceType) =>
        val back         = journeyService.previous(routes.TaxSituationController.taxSituation())
        val reportIncome =
          request.sessionData.userAnswers.fold(_.reportIncomeEarned, c => Some(c.reportIncomeEarned))
        val options      = reportEarnedIncomeOptions(licenceType)
        val form = {
          val emptyForm = reportIncomeForm(options)
          reportIncome.fold(emptyForm)(emptyForm.fill)
        }
        Ok(reportIncomeEarnedPage(form, back, options))
      case None              =>
        logger.error("Couldn't find licence Type")
        InternalServerError
    }
  }

  val taxSituationSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleReportedIncome(reportIncomeEarned: ReportIncomeEarned): Future[Result] = {
      val updatedAnswers =
        request.sessionData.userAnswers
          .unset(_.reportIncomeEarned)
          .copy(reportIncomeEarned = Some(reportIncomeEarned))
      journeyService
        .updateAndNext(
          routes.TaxSituationController.taxSituation(),
          request.sessionData.copy(userAnswers = updatedAnswers)
        )
        .fold(
          { e =>
            logger.warn("Could not update session and proceed", e)
            InternalServerError
          },
          Redirect
        )
    }

    val licenceTypeOpt = request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType))
    licenceTypeOpt match {
      case Some(licenceType) =>
        val options = reportEarnedIncomeOptions(licenceType)
        reportIncomeForm(options)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                reportIncomeEarnedPage(
                  formWithErrors,
                  journeyService.previous(routes.TaxSituationController.taxSituation()),
                  options
                )
              ),
            handleReportedIncome
          )
      case None              =>
        logger.error("Couldn't find licence Type")
        InternalServerError
    }
  }

}

object TaxSituationController {
  val reportIncomeList: List[ReportIncomeEarned] = List(
    PAYE,
    SA,
    SAPAYE,
    NotChargeable
  )

  def reportEarnedIncomeOptions(licenceType: LicenceType): List[ReportIncomeEarned] =
    licenceType match {
      case DriverOfTaxisAndPrivateHires => reportIncomeList
      case _                            => List(reportIncomeList(0), reportIncomeList(2))
    }

  def reportIncomeForm(options: List[ReportIncomeEarned]): Form[ReportIncomeEarned] =
    Form(
      mapping(
        "reportIncomeEarned" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )
}
