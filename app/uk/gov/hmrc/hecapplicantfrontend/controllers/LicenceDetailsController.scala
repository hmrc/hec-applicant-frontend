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
import play.api.i18n.Messages
import play.api.data.Forms.{mapping, of}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.LicenceDetailsController.{licenceTypeForm, licenceTypeOptions, licenseExpiryDateForm}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{LicenceExpiryDate, LicenceType}
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType._
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeUtils}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LicenceDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  mcc: MessagesControllerComponents,
  licenceTypePage: html.LicenceType,
  licenceTypeExitPage: html.LicenceTypeExit,
  licenseExpiryDatePage: html.LicenceExpiryDate,
  licenceExpiryDateExitPage: html.LicenceExpiryDateExit
)(implicit appConfig: AppConfig, ec: ExecutionContext)
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
    def handleValidLicenceType(licenceType: LicenceType): Future[Result] = {
      val updatedAnswers =
        request.sessionData.userAnswers.unset(_.licenceType).copy(licenceType = Some(licenceType))
      journeyService
        .updateAndNext(
          routes.LicenceDetailsController.licenceType(),
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

    licenceTypeForm(licenceTypeOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            licenceTypePage(
              formWithErrors,
              journeyService.previous(routes.LicenceDetailsController.licenceType()),
              licenceTypeOptions
            )
          ),
        handleValidLicenceType
      )
  }

  val licenceTypeExit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      licenceTypeExitPage(
        journeyService.previous(routes.LicenceDetailsController.licenceTypeExit()),
        licenceTypeOptions
      )
    )
  }

  val expiryDate: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back       = journeyService.previous(routes.LicenceDetailsController.expiryDate())
    val expiryDate = request.sessionData.userAnswers.fold(_.licenceExpiryDate, c => Some(c.licenceExpiryDate))
    val form = {
      val emptyForm = licenseExpiryDateForm()
      expiryDate.fold(emptyForm)(emptyForm.fill)
    }
    Ok(licenseExpiryDatePage(form, back))
  }

  val expiryDateSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    lazy val back = journeyService.previous(routes.LicenceDetailsController.expiryDate())
    licenseExpiryDateForm()
      .bindFromRequest()
      .fold(
        formWithErrors => Ok(licenseExpiryDatePage(formWithErrors, back)),
        { licenceExpiryDate =>
          val updatedAnswers =
            request.sessionData.userAnswers.unset(_.licenceExpiryDate).copy(licenceExpiryDate = Some(licenceExpiryDate))
          val updatedSession = request.sessionData.copy(userAnswers = updatedAnswers)
          journeyService
            .updateAndNext(
              routes.LicenceDetailsController.expiryDate(),
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

  val expiryDateExit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      licenceExpiryDateExitPage(
        journeyService.previous(routes.LicenceDetailsController.expiryDateExit())
      )
    )
  }

  val timeTrading: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      s"session is ${request.sessionData}\nBack is ${journeyService.previous(routes.LicenceDetailsController.timeTrading())}"
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

  def licenseExpiryDateForm()(implicit message: Messages): Form[LicenceExpiryDate] = {
    val key                             = "licenceExpiryDate"
    val tooFarInFutureDate              = TimeUtils.today().plusYears(6L)
    val tooFarInPastDate                = TimeUtils.today().minusYears(2L)
    val tooFarInFutureArgs: Seq[String] = Seq(TimeUtils.govDisplayFormat(tooFarInFutureDate))
    val tooFarInPastArgs                = Seq(TimeUtils.govDisplayFormat(tooFarInPastDate))
    Form(
      mapping(
        "" -> of(
          TimeUtils.dateFormatter(
            Some(tooFarInFutureDate),
            Some(tooFarInPastDate),
            s"$key-day",
            s"$key-month",
            s"$key-year",
            key,
            tooFarInFutureArgs = tooFarInFutureArgs,
            tooFarInPastArgs = tooFarInPastArgs
          )
        )
      )(LicenceExpiryDate(_))(d => Some(d.value))
    )
  }

}
