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

import java.time.LocalDate

import cats.data.EitherT
import uk.gov.hmrc.hecapplicantfrontend.models
import uk.gov.hmrc.hecapplicantfrontend.models.SAStatusResponse
import cats.implicits._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.{getTaxYear, taxSituationForm, taxSituationOptions}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.IndividualRetrievedData
import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.{TaxSituation, TaxYear}
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider, TimeUtils}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.models.Error

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaxSituationController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  taxCheckService: TaxCheckService,
  mcc: MessagesControllerComponents,
  timeProvider: TimeProvider,
  taxSituationPage: html.TaxSituation
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val taxSituation: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val licenceTypeOpt = request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType))
    licenceTypeOpt match {
      case Some(licenceType) =>
        val back         = journeyService.previous(routes.TaxSituationController.taxSituation())
        val reportIncome = request.sessionData.userAnswers.fold(_.taxSituation, c => Some(c.taxSituation))
        val options      = taxSituationOptions(licenceType)
        val form = {
          val emptyForm = taxSituationForm(options)
          reportIncome.fold(emptyForm)(emptyForm.fill)
        }

        Ok(taxSituationPage(form, back, options, getTaxYear(timeProvider.currentDate)))
      case None              =>
        logger.error("Couldn't find licence Type")
        InternalServerError
    }
  }

  private val saTaxSituations = Seq(TaxSituation.SA, TaxSituation.SAPAYE)

  val taxSituationSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val taxYear = getTaxYear(TimeUtils.today())

    def fetchSAStatus(
      individualRetrievedData: IndividualRetrievedData,
      taxSituation: TaxSituation
    ): EitherT[Future, models.Error, Option[SAStatusResponse]] =
      if (saTaxSituations.contains(taxSituation)) {
        individualRetrievedData.sautr
          .map(taxCheckService.getSAStatus(_, taxYear))
          .sequence[EitherT[Future, Error, *], SAStatusResponse]
      } else {
        EitherT.pure[Future, models.Error](None)
      }

    def handleValidTaxSituation(taxSituation: TaxSituation): Future[Result] =
      request.sessionData.retrievedUserData match {
        case individualRetrievedData: IndividualRetrievedData =>
          val result = for {
            maybeSaStatus <- fetchSAStatus(individualRetrievedData, taxSituation)

            updatedRetrievedData = individualRetrievedData.copy(saStatus = maybeSaStatus)
            updatedAnswers       =
              request.sessionData.userAnswers.unset(_.taxSituation).copy(taxSituation = Some(taxSituation))
            updatedRequest       = request.sessionData.copy(
                                     userAnswers = updatedAnswers,
                                     retrievedUserData = updatedRetrievedData
                                   )

            next <- journeyService.updateAndNext(routes.TaxSituationController.taxSituation(), updatedRequest)
          } yield next

          result.fold(
            { e =>
              logger.warn("Fetch SA status failed or could not update session and proceed", e)
              InternalServerError
            },
            Redirect
          )
        case _                                                =>
          logger.error("Companies should not see this page")
          Future.successful(InternalServerError)
      }

    val licenceTypeOpt = request.sessionData.userAnswers.fold(_.licenceType, c => Some(c.licenceType))
    licenceTypeOpt match {
      case Some(licenceType) =>
        val options = taxSituationOptions(licenceType)
        taxSituationForm(options)
          .bindFromRequest()
          .fold(
            formWithErrors =>
              Ok(
                taxSituationPage(
                  formWithErrors,
                  journeyService.previous(routes.TaxSituationController.taxSituation()),
                  options,
                  taxYear
                )
              ),
            handleValidTaxSituation
          )
      case None              =>
        logger.error("Couldn't find licence type")
        InternalServerError
    }
  }

  // Placeholder for confirm your income (HEC-985)
  val confirmYourIncome: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      s"Session is ${request.sessionData} back Url ::${journeyService.previous(routes.TaxSituationController.confirmYourIncome())}"
    )
  }

  // Placeholder for exit page when no tax return is found (HEC-987)
  val noReturnFoundExit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      s"Session is ${request.sessionData} back Url ::${journeyService.previous(routes.TaxSituationController.noReturnFoundExit())}"
    )
  }

  // Placeholder for exit page when SAUTR not found (HEC-986)
  val sautrNotFoundExit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(
      s"Session is ${request.sessionData} back Url ::${journeyService.previous(routes.TaxSituationController.sautrNotFoundExit())}"
    )
  }

}

object TaxSituationController {
  private val allTaxSituations: List[TaxSituation] = List(
    PAYE,
    SA,
    SAPAYE,
    NotChargeable
  )

  private val nonPAYETaxSituations = List(SA, NotChargeable)

  def taxSituationOptions(licenceType: LicenceType): List[TaxSituation] =
    licenceType match {
      case LicenceType.DriverOfTaxisAndPrivateHires => allTaxSituations
      case LicenceType.OperatorOfPrivateHireVehicles | LicenceType.ScrapMetalMobileCollector |
          LicenceType.ScrapMetalDealerSite =>
        nonPAYETaxSituations
    }

  def taxSituationForm(options: List[TaxSituation]): Form[TaxSituation] =
    Form(
      mapping(
        "taxSituation" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )

  def getTaxYear(currentDate: LocalDate): TaxYear = {
    val currentYear             = currentDate.getYear
    val currentYearTaxStartDate = LocalDate.of(currentYear, 4, 6)
    val sixMonthEarlierDate     = currentDate.minusMonths(6L)
    if (sixMonthEarlierDate.isBefore(currentYearTaxStartDate)) TaxYear(currentYear - 2)
    else TaxYear(currentYear - 1)
  }

}
