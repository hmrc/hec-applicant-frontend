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
import uk.gov.hmrc.hecapplicantfrontend.models.YesNoAnswer
import uk.gov.hmrc.hecapplicantfrontend.models.views.YesNoOption
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
  saIncomeStatementPage: html.SAIncomeStatement
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val saIncomeStatement: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back             = journeyService.previous(routes.SAController.saIncomeStatement())
    val saIncomeDeclared = request.sessionData.userAnswers.fold(_.saIncomeDeclared, _.saIncomeDeclared)
    val form = {
      val emptyForm = SAController.saIncomeDeclarationForm(YesNoAnswer.values)
      saIncomeDeclared.fold(emptyForm)(emptyForm.fill)
    }
    Ok(saIncomeStatementPage(form, back, SAController.incomeDeclaredOptions, getTaxYear(timeProvider.currentDate)))
  }

  val saIncomeStatementSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.mapAsIndividual { individualSession =>
      def handleValidAnswer(incomeDeclared: YesNoAnswer): Future[Result] = {
        val updatedAnswers =
          individualSession.userAnswers.unset(_.saIncomeDeclared).copy(saIncomeDeclared = Some(incomeDeclared))

        journeyService
          .updateAndNext(
            routes.SAController.saIncomeStatement(),
            individualSession.copy(userAnswers = updatedAnswers)
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
        .saIncomeDeclarationForm(YesNoAnswer.values)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              saIncomeStatementPage(
                formWithErrors,
                journeyService.previous(routes.SAController.saIncomeStatement()),
                SAController.incomeDeclaredOptions,
                getTaxYear(timeProvider.currentDate)
              )
            ),
          handleValidAnswer
        )
    }
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

  val incomeDeclaredOptions: List[YesNoOption] = YesNoAnswer.values.map(YesNoOption.yesNoOption)

  def saIncomeDeclarationForm(options: List[YesNoAnswer]): Form[YesNoAnswer] =
    Form(
      mapping(
        "saIncomeDeclared" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

}
