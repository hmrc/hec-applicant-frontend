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
import uk.gov.hmrc.hecapplicantfrontend.controllers.LicenceDetailsController._
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceTimeTrading._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
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
  licenceTimeTradingPage: html.LicenceTimeTrading,
  licenceValidityPeriodPage: html.LicenceValidityPeriod
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
        if (request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType)).contains(licenceType))
          request.sessionData.userAnswers
        else
          UserAnswers.empty.copy(licenceType = Some(licenceType))

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

  val licenceTimeTrading: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back        = journeyService.previous(routes.LicenceDetailsController.licenceTimeTrading())
    val timeTrading = request.sessionData.userAnswers.fold(_.licenceTimeTrading, c => Some(c.licenceTimeTrading))
    val form = {
      val emptyForm = licenceTimeTradingForm(licenceTimeTradingOptions)
      timeTrading.fold(emptyForm)(emptyForm.fill)
    }
    Ok(licenceTimeTradingPage(form, back, licenceTimeTradingOptions))
  }

  val licenceTimeTradingSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleValidLicenceTimeTrading(licenceTimeTrading: LicenceTimeTrading): Future[Result] = {
      val updatedAnswers =
        request.sessionData.userAnswers.unset(_.licenceTimeTrading).copy(licenceTimeTrading = Some(licenceTimeTrading))
      journeyService
        .updateAndNext(
          routes.LicenceDetailsController.licenceTimeTrading(),
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

    licenceTimeTradingForm(licenceTimeTradingOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            licenceTimeTradingPage(
              formWithErrors,
              journeyService.previous(routes.LicenceDetailsController.licenceTimeTrading()),
              licenceTimeTradingOptions
            )
          ),
        handleValidLicenceTimeTrading
      )
  }

  val recentLicenceLength: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val licenceTypeOpt = request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType))
    licenceTypeOpt match {
      case Some(licenceType) =>
        val back          = journeyService.previous(routes.LicenceDetailsController.recentLicenceLength())
        val licenceLength =
          request.sessionData.userAnswers.fold(_.licenceValidityPeriod, c => Some(c.licenceValidityPeriod))
        val options       = licenceValidityPeriodOptions(licenceType)
        val form = {
          val emptyForm = licenceValidityPeriodForm(options)
          licenceLength.fold(emptyForm)(emptyForm.fill)
        }
        Ok(licenceValidityPeriodPage(form, back, options))
      case None              =>
        logger.error("Couldn't find licence Type")
        InternalServerError
    }

  }

  val recentLicenceLengthSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleValidLicenceTimePeriod(licenceValidityPeriod: LicenceValidityPeriod): Future[Result] = {
      val updatedAnswers =
        request.sessionData.userAnswers
          .unset(_.licenceValidityPeriod)
          .copy(licenceValidityPeriod = Some(licenceValidityPeriod))
      journeyService
        .updateAndNext(
          routes.LicenceDetailsController.recentLicenceLength(),
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
        val options: List[LicenceValidityPeriod] = licenceValidityPeriodOptions(licenceType)
        licenceValidityPeriodForm(options)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                licenceValidityPeriodPage(
                  formWithErrors,
                  journeyService.previous(routes.LicenceDetailsController.recentLicenceLength()),
                  options
                )
              ),
            handleValidLicenceTimePeriod
          )
      case None              =>
        logger.error("Couldn't find licence Type")
        InternalServerError
    }
  }

}

object LicenceDetailsController {

  val licenceTypeOptions: List[LicenceType] = List(
    DriverOfTaxisAndPrivateHires,
    OperatorOfPrivateHireVehicles,
    ScrapMetalMobileCollector,
    ScrapMetalDealerSite
  )

  val licenceTimeTradingOptions: List[LicenceTimeTrading] = List(
    ZeroToTwoYears,
    TwoToFourYears,
    FourToEightYears,
    EightYearsOrMore
  )

  private val validityPeriodList = List(UpToOneYear, UpToTwoYears, UpToThreeYears, UpToFourYears, UpToFiveYears)

  def licenceValidityPeriodOptions(licenceType: LicenceType): List[LicenceValidityPeriod] =
    licenceType match {
      case OperatorOfPrivateHireVehicles => validityPeriodList
      case _                             => validityPeriodList.take(3)
    }

  def licenceTypeForm(options: List[LicenceType]): Form[LicenceType] =
    Form(
      mapping(
        "licenceType" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

  def licenceTimeTradingForm(options: List[LicenceTimeTrading]): Form[LicenceTimeTrading] =
    Form(
      mapping(
        "licenceTimeTrading" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

  def licenceValidityPeriodForm(options: List[LicenceValidityPeriod]): Form[LicenceValidityPeriod] =
    Form(
      mapping(
        "licenceValidityPeriod" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

}
