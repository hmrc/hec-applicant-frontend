/*
 * Copyright 2022 HM Revenue & Customs
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

import com.google.inject.{Inject, Singleton}
import cats.instances.future._
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.{HECTaxCheck, TaxCheckListItem}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.CompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.views.html

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaxCheckCompleteController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  taxCheckCompletePage: html.TaxCheckComplete
)(implicit ec: ExecutionContext, appConfig: AppConfig)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  /**
    * Fetches tax check data (code & expiry date) for authenticated user
    */
  val taxCheckComplete: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    ensureCompletedTaxCheckAndLicenceType { case (completedTaxCheck, licenceType) =>
      Ok(taxCheckCompletePage(completedTaxCheck, licenceType))
    }
  }

  val emailTaxCheckCode: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    if (!appConfig.sendEmailEnabled)
      sys.error("Email journey is not enabled")
    else {
      ensureCompletedTaxCheckAndLicenceType { case (completedTaxCheck, licenceType) =>
        val session                   = request.sessionData
        val emailRequestedForTaxCheck = TaxCheckListItem(
          licenceType,
          completedTaxCheck.taxCheckCode,
          completedTaxCheck.expiresAfter,
          completedTaxCheck.createDate
        )
        val updatedSession            = session.fold(
          _.copy(emailRequestedForTaxCheck = Some(emailRequestedForTaxCheck)),
          _.copy(emailRequestedForTaxCheck = Some(emailRequestedForTaxCheck))
        )
        journeyService
          .updateAndNext(
            routes.TaxCheckCompleteController.taxCheckComplete(),
            updatedSession
          )
          .fold(
            _.doThrow("Could not update session and proceed"),
            Redirect
          )
      }

    }
  }

  private def ensureCompletedTaxCheckAndLicenceType(
    f: (HECTaxCheck, LicenceType) => Future[Result]
  )(implicit r: RequestWithSessionData[_]): Future[Result] = {
    val licenceType = r.sessionData.userAnswers.foldByCompleteness(
      _ => sys.error("Could not find complete answers"),
      {
        case ci: CompleteIndividualUserAnswers => ci.licenceType
        case cc: CompleteCompanyUserAnswers    => cc.licenceType
      }
    )

    r.sessionData.completedTaxCheck match {
      case Some(taxCheck) => f(taxCheck, licenceType)
      case None           => sys.error("Completed tax check not found")
    }
  }

}
