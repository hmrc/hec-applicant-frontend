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
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.CompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.{CompleteUserAnswers, CompleteUserAnswersCombined}
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class CheckYourAnswersController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  taxCheckService: TaxCheckService,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  checkYourAnswersPage: html.CheckYourAnswers
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private def toCompleteUserAnswersCombined(c: CompleteUserAnswers) = c match {
    case i: CompleteIndividualUserAnswers => CompleteUserAnswersCombined.fromIndividualAnswers(i)
    case i: CompleteCompanyUserAnswers    => CompleteUserAnswersCombined.fromCompanyAnswers(i)
  }

  val checkYourAnswers: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.userAnswers.foldByCompleteness(
      { _ =>
        logger.warn("Could not find complete answers")
        InternalServerError
      },
      { complete =>
        val back            = journeyService.previous(routes.CheckYourAnswersController.checkYourAnswers())
        val combinedAnswers = toCompleteUserAnswersCombined(complete)
        Ok(checkYourAnswersPage(back, combinedAnswers, request.sessionData.retrievedJourneyData))
      }
    )
  }

  val checkYourAnswersSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.userAnswers match {
      case c: CompleteUserAnswers =>
        val combinedAnswers = toCompleteUserAnswersCombined(c)
        val result          = for {
          taxCheck      <- taxCheckService.saveTaxCheck(request.sessionData, combinedAnswers)
          updatedSession = request.sessionData.fold(
                             _.copy(completedTaxCheck = Some(taxCheck)),
                             _.copy(completedTaxCheck = Some(taxCheck))
                           )
          next          <- journeyService.updateAndNext(routes.CheckYourAnswersController.checkYourAnswers(), updatedSession)
        } yield next

        result.fold(
          { e =>
            logger.warn("Could not save tax check", e)
            InternalServerError
          },
          Redirect
        )

      case _ =>
        logger.warn("Could not find complete answers")
        InternalServerError
    }
  }

}
