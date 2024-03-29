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
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.CompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
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
  checkYourAnswersIndividualPage: html.CheckYourAnswersIndividual,
  checkYourAnswersCompanyPage: html.CheckYourAnswersCompany
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val checkYourAnswers: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.userAnswers.foldByCompleteness(
      _ => InconsistentSessionState("Could not find complete answers").doThrow,
      { complete =>
        val back = journeyService.previous(routes.CheckYourAnswersController.checkYourAnswers)
        complete match {
          case ci: CompleteIndividualUserAnswers =>
            Ok(
              checkYourAnswersIndividualPage(
                back,
                ci,
                request.sessionData.loginData,
                request.sessionData
                  .mapAsIndividual(_.relevantIncomeTaxYear)
                  .getOrElse(InconsistentSessionState("Could not find relevant income tax year").doThrow)
              )
            )
          case cc: CompleteCompanyUserAnswers    =>
            Ok(
              checkYourAnswersCompanyPage(
                back,
                cc,
                request.sessionData.retrievedJourneyData,
                request.sessionData.loginData
              )
            )
        }
      }
    )
  }

  val checkYourAnswersSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.userAnswers match {
      case c: CompleteUserAnswers =>
        val result = for {
          taxCheck      <-
            taxCheckService.saveTaxCheck(request.sessionData, c, request.language)
          updatedSession = request.sessionData.fold(
                             _.copy(completedTaxCheck = Some(taxCheck)),
                             _.copy(completedTaxCheck = Some(taxCheck))
                           )
          next          <- journeyService.updateAndNext(routes.CheckYourAnswersController.checkYourAnswers, updatedSession)
        } yield next

        result.fold(
          _.doThrow("Could not save tax check"),
          Redirect
        )

      case _ =>
        InconsistentSessionState("Could not find complete answers").doThrow
    }
  }

}
