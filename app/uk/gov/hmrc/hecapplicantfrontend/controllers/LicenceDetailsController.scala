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

import cats.implicits.catsSyntaxEq
import cats.instances.future._
import cats.syntax.option._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.LicenceDetailsController._
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceTimeTrading._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyUserAnswers, HECSession, IndividualUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class LicenceDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  timeProvider: TimeProvider,
  mcc: MessagesControllerComponents,
  licenceTypePage: html.LicenceType,
  licenceTypeExitPage: html.LicenceTypeExit,
  licenceTimeTradingPage: html.LicenceTimeTrading,
  licenceValidityPeriodPage: html.LicenceValidityPeriod,
  maxTaxChecksLimitExceededPage: html.MaxTaxChecksLimitExceeded
)(implicit appConfig: AppConfig, ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val licenceType: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back        = journeyService.previous(routes.LicenceDetailsController.licenceType)
    val licenceType = request.sessionData.userAnswers.fold(
      _.fold(_.licenceType, _.licenceType.some),
      _.fold(_.licenceType, _.licenceType.some)
    )

    val licenceOptions = licenceTypeOptions(request.sessionData)
    val form = {
      val emptyForm = licenceTypeForm(licenceOptions)
      licenceType.fold(emptyForm)(emptyForm.fill)
    }
    Ok(licenceTypePage(form, back, licenceOptions))
  }

  val maxTaxChecksExceeded: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(maxTaxChecksLimitExceededPage())
  }

  val licenceTypeSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val licenceOptions = licenceTypeOptions(request.sessionData)
    def handleValidLicenceType(licenceType: LicenceType): Future[Result] = {
      val taxCheckStartDateTime = request.sessionData.taxCheckStartDateTime.getOrElse(timeProvider.now)
      val licenceTypeAnswer     = request.sessionData.userAnswers.fold(
        _.fold(_.licenceType, _.licenceType.some),
        _.fold(_.licenceType, _.licenceType.some)
      )
      val updatedSession        =
        if (licenceTypeAnswer.contains(licenceType))
          request.sessionData
        else
          request.sessionData.fold(
            { individualSession =>
              val answers = IndividualUserAnswers.empty.copy(licenceType = Some(licenceType))
              individualSession.copy(userAnswers = answers, taxCheckStartDateTime = Some(taxCheckStartDateTime))
            },
            { companySession =>
              val answers = CompanyUserAnswers.empty.copy(licenceType = Some(licenceType))
              companySession.copy(userAnswers = answers, taxCheckStartDateTime = Some(taxCheckStartDateTime))
            }
          )
      updateAndNextJourneyData(
        routes.LicenceDetailsController.licenceType,
        updatedSession
      )
    }

    licenceTypeForm(licenceOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            licenceTypePage(
              formWithErrors,
              journeyService.previous(routes.LicenceDetailsController.licenceType),
              licenceOptions
            )
          ),
        handleValidLicenceType
      )
  }

  val licenceTypeExit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val licenceOptions = licenceTypeOptions(request.sessionData)
    Ok(
      licenceTypeExitPage(
        journeyService.previous(routes.LicenceDetailsController.licenceTypeExit),
        licenceOptions
      )
    )
  }

  val licenceTimeTrading: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val back        = journeyService.previous(routes.LicenceDetailsController.licenceTimeTrading)
    val timeTrading = request.sessionData.userAnswers.fold(
      _.fold(_.licenceTimeTrading, _.licenceTimeTrading.some),
      _.fold(_.licenceTimeTrading, _.licenceTimeTrading.some)
    )
    val form = {
      val emptyForm: Form[LicenceTimeTrading] = licenceTimeTradingForm(licenceTimeTradingOptions)
      timeTrading.fold(emptyForm)(emptyForm.fill)
    }
    Ok(licenceTimeTradingPage(form, back, licenceTimeTradingOptions))
  }

  val licenceTimeTradingSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleValidLicenceTimeTrading(licenceTimeTrading: LicenceTimeTrading): Future[Result] = {
      val updatedSession = request.sessionData.replaceField(
        request.sessionData,
        IncompleteIndividualUserAnswers.licenceTimeTrading,
        IncompleteCompanyUserAnswers.licenceTimeTrading,
        _.copy(licenceTimeTrading = Some(licenceTimeTrading)),
        _.copy(licenceTimeTrading = Some(licenceTimeTrading))
      )

      updateAndNextJourneyData(
        routes.LicenceDetailsController.licenceTimeTrading,
        updatedSession
      )
    }

    licenceTimeTradingForm(licenceTimeTradingOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            licenceTimeTradingPage(
              formWithErrors,
              journeyService.previous(routes.LicenceDetailsController.licenceTimeTrading),
              licenceTimeTradingOptions
            )
          ),
        handleValidLicenceTimeTrading
      )
  }

  val recentLicenceLength: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val licenceTypeOpt = request.sessionData.userAnswers.fold(
      _.fold(_.licenceType, _.licenceType.some),
      _.fold(_.licenceType, _.licenceType.some)
    )
    licenceTypeOpt match {
      case Some(licenceType) =>
        val back          = journeyService.previous(routes.LicenceDetailsController.recentLicenceLength)
        val licenceLength =
          request.sessionData.userAnswers.fold(
            _.fold(_.licenceValidityPeriod, _.licenceValidityPeriod.some),
            _.fold(_.licenceValidityPeriod, _.licenceValidityPeriod.some)
          )
        val options       = licenceValidityPeriodOptions(licenceType)
        val form = {
          val emptyForm = licenceValidityPeriodForm(options)
          licenceLength.fold(emptyForm)(emptyForm.fill)
        }
        Ok(licenceValidityPeriodPage(form, back, options))
      case None              =>
        InconsistentSessionState("Couldn't find licence Type").doThrow
    }

  }

  val recentLicenceLengthSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    def handleValidLicenceTimePeriod(licenceValidityPeriod: LicenceValidityPeriod): Future[Result] = {
      val updatedSession = request.sessionData.replaceField(
        request.sessionData,
        IncompleteIndividualUserAnswers.licenceValidityPeriod,
        IncompleteCompanyUserAnswers.licenceValidityPeriod,
        _.copy(licenceValidityPeriod = Some(licenceValidityPeriod)),
        _.copy(licenceValidityPeriod = Some(licenceValidityPeriod))
      )

      updateAndNextJourneyData(
        routes.LicenceDetailsController.recentLicenceLength,
        updatedSession
      )
    }
    val licenceTypeOpt = request.sessionData.userAnswers.fold(
      _.fold(_.licenceType, _.licenceType.some),
      _.fold(_.licenceType, _.licenceType.some)
    )
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
                  journeyService.previous(routes.LicenceDetailsController.recentLicenceLength),
                  options
                )
              ),
            handleValidLicenceTimePeriod
          )
      case None              =>
        InconsistentSessionState("Couldn't find licence Type").doThrow
    }
  }

  private def updateAndNextJourneyData(current: Call, updatedSession: HECSession)(implicit
    r: RequestWithSessionData[_],
    hc: HeaderCarrier
  ): Future[Result] =
    journeyService
      .updateAndNext(
        current,
        updatedSession
      )
      .fold(
        _.doThrow("Could not update session and proceed"),
        Redirect
      )

}

object LicenceDetailsController {

  val licenceTypes: List[LicenceType] = List(
    DriverOfTaxisAndPrivateHires,
    OperatorOfPrivateHireVehicles,
    ScrapMetalMobileCollector,
    ScrapMetalDealerSite
  )

  val individualLicenceTypeOptions: List[LicenceType] = licenceTypes
  val companyLicenceTypeOptions: List[LicenceType]    = licenceTypes.filter(_ =!= DriverOfTaxisAndPrivateHires)

  val licenceTimeTradingOptions: List[LicenceTimeTrading] = List(
    ZeroToTwoYears,
    TwoToFourYears,
    FourToEightYears,
    EightYearsOrMore
  )

  private val validityPeriodList = List(UpToOneYear, UpToTwoYears, UpToThreeYears, UpToFourYears, UpToFiveYears)

  def licenceTypeOptions(session: HECSession): List[LicenceType] = session.loginData match {
    case _: IndividualLoginData => individualLicenceTypeOptions
    case _: CompanyLoginData    => companyLicenceTypeOptions
  }

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
