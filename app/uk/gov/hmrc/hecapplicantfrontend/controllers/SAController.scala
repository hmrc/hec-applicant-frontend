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
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.getTaxYear
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.IncomeConfirmation
import uk.gov.hmrc.hecapplicantfrontend.models.views.IncomeConfirmationOption
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class SAController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  timeProvider: TimeProvider,
  sautrNotFoundPage: html.SautrNotFound,
  noReturnFoundPage: html.NoReturnFound,
  confirmYourIncomePage: html.ConfirmYourIncome
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmYourIncome: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back               = journeyService.previous(routes.SAController.confirmYourIncome())
    val incomeConfirmation = request.sessionData.userAnswers.fold(_.incomeConfirmation, c => Some(c.incomeConfirmation))
    val form = {
      val emptyForm = SAController.incomeConfirmationForm(IncomeConfirmation.values)
      incomeConfirmation.fold(emptyForm)(emptyForm.fill)
    }
    Ok(confirmYourIncomePage(form, back, SAController.incomeConfirmationOptions, getTaxYear(timeProvider.currentDate)))
  }

  val confirmYourIncomeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleValidAnswer(incomeConfirmation: IncomeConfirmation): Future[Result] = {
      val updatedAnswers =
        request.sessionData.userAnswers.unset(_.incomeConfirmation).copy(incomeConfirmation = Some(incomeConfirmation))

      journeyService
        .updateAndNext(
          routes.SAController.confirmYourIncome(),
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

    SAController
      .incomeConfirmationForm(IncomeConfirmation.values)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            confirmYourIncomePage(
              formWithErrors,
              journeyService.previous(routes.SAController.confirmYourIncome()),
              SAController.incomeConfirmationOptions,
              getTaxYear(timeProvider.currentDate)
            )
          ),
        handleValidAnswer
      )
  }

  val noReturnFound: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back = journeyService.previous(routes.SAController.noReturnFound())
    Ok(noReturnFoundPage(back))
  }

  val sautrNotFound: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back = journeyService.previous(routes.SAController.sautrNotFound())
    Ok(sautrNotFoundPage(back))
  }

}

object SAController {

  val incomeConfirmationOptions: List[IncomeConfirmationOption] =
    IncomeConfirmation.values.map(IncomeConfirmationOption.incomeConfirmationOption)

  def incomeConfirmationForm(options: List[IncomeConfirmation]): Form[IncomeConfirmation] =
    Form(
      mapping(
        "confirmYourIncome" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

}
