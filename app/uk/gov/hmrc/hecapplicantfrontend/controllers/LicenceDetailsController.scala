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
import play.api.i18n.I18nSupport
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.LicenceDetailsController.{licenceTypeForm, licenceTypeOptions}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType._
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class LicenceDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  licenceTypePage: html.LicenceType,
  licenceTypeExitPage: html.LicenceTypeExit
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val licenceType: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back        = journeyService.previous(routes.LicenceDetailsController.licenceType())
    val licenceType = request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType))
    val form = {
      val emptyForm = licenceTypeForm(licenceTypeOptions)
      licenceType.fold(emptyForm)(emptyForm.fill)
    }

    Ok(licenceTypePage(form, back, licenceTypeOptions))
  }

  val licenceTypeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    lazy val back = journeyService.previous(routes.LicenceDetailsController.licenceType())
    licenceTypeForm(licenceTypeOptions)
      .bindFromRequest()
      .fold(
        formWithErrors => Ok(licenceTypePage(formWithErrors, back, licenceTypeOptions)),
        { licenceType =>
          val updatedAnswers =
            request.sessionData.userAnswers.unset(_.licenceType).copy(licenceType = Some(licenceType))
          val updatedSession = request.sessionData.copy(userAnswers = updatedAnswers)
          journeyService
            .updateAndNext(
              routes.LicenceDetailsController.licenceType(),
              updatedSession
            )
            .fold(
              { e =>
                logger.warn("Could not update session and proceed", e)
                InternalServerError
              },
              Redirect
            )

        }
      )
  }

  val licenceTypeExit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      licenceTypeExitPage(journeyService.previous(routes.LicenceDetailsController.licenceTypeExit()))
    )
  }

  val expiryDate: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      s"session is ${request.sessionData}\nBack is ${journeyService.previous(routes.LicenceDetailsController.licenceType())}"
    )
  }

}

object LicenceDetailsController {

  val licenceTypeOptions: List[LicenceType] = List(
    DriverOfTaxisAndPrivateHires,
    OperatorOfPrivateHireVehicles,
    ScrapMetalMobileCollector,
    ScrapMetalDealerSite
  )

  def licenceTypeForm(options: List[LicenceType]): Form[LicenceType] =
    Form(
      mapping(
        "licenceType" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

}
