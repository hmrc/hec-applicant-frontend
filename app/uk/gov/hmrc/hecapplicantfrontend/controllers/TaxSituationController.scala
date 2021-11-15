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

import cats.data.EitherT
import cats.implicits._
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.{I18nSupport, Messages}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController._
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, SAStatusResponse, TaxSituation, TaxYear}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeProvider, TimeUtils}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDate
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaxSituationController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  sessionStore: SessionStore,
  journeyService: JourneyService,
  taxCheckService: TaxCheckService,
  mcc: MessagesControllerComponents,
  timeProvider: TimeProvider,
  taxSituationPage: html.TaxSituation
)(implicit ec: ExecutionContext, appConfig: AppConfig)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val taxSituation: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.mapAsIndividual { implicit individualSession =>
      request.sessionData.ensureLicenceTypePresent { licenceType =>
        val back            = journeyService.previous(routes.TaxSituationController.taxSituation())
        val taxSituationOpt = individualSession.userAnswers.fold(_.taxSituation, _.taxSituation.some)
        val options         = taxSituationOptions(licenceType)
        val form = {
          val emptyForm = taxSituationForm(options)
          taxSituationOpt.fold(emptyForm)(emptyForm.fill)
        }

        individualSession.relevantIncomeTaxYear match {
          case Some(taxYear) =>
            val (startDate, endDate) = getTaxPeriodStrings(taxYear)
            Ok(taxSituationPage(form, back, options, startDate, endDate))
          case None          =>
            val taxYear              = getTaxYear(timeProvider.currentDate)
            val (startDate, endDate) = getTaxPeriodStrings(taxYear)
            val updatedSession       = individualSession.copy(relevantIncomeTaxYear = Some(taxYear))
            sessionStore
              .store(updatedSession)
              .fold(
                _.doThrow("Could not update session with tax year"),
                _ => Ok(taxSituationPage(form, back, options, startDate, endDate))
              )
        }

      }
    }
  }

  val taxSituationSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.mapAsIndividual { individualSession =>
      request.sessionData.ensureLicenceTypePresent { licenceType =>
        val taxYear                                                = individualSession.relevantIncomeTaxYear.getOrElse(getTaxYear(timeProvider.currentDate))
        def fetchSAStatus(
          individualLoginData: IndividualLoginData,
          taxSituation: TaxSituation
        ): EitherT[Future, models.Error, Option[SAStatusResponse]] =
          if (saTaxSituations.contains(taxSituation)) {
            individualLoginData.sautr
              .traverse[EitherT[Future, Error, *], SAStatusResponse](taxCheckService.getSAStatus(_, taxYear))
          } else {
            EitherT.pure[Future, models.Error](None)
          }

        def handleValidTaxSituation(taxSituation: TaxSituation): Future[Result] = {
          val result = for {
            maybeSaStatus <- fetchSAStatus(individualSession.loginData, taxSituation)

            updatedRetrievedData = individualSession.retrievedJourneyData.copy(saStatus = maybeSaStatus)
            // wipe the SA income declared answer since it is dependent on the tax situation answer
            updatedAnswers       = individualSession.userAnswers
                                     .unset(_.taxSituation)
                                     .unset(_.saIncomeDeclared)
                                     .copy(taxSituation = Some(taxSituation))
            updatedRequest       = individualSession.copy(
                                     userAnswers = updatedAnswers,
                                     retrievedJourneyData = updatedRetrievedData
                                   )

            next <- journeyService.updateAndNext(routes.TaxSituationController.taxSituation(), updatedRequest)
          } yield next

          result.fold(
            _.doThrow("Fetch SA status failed or could not update session and proceed"),
            Redirect
          )
        }

        val options = taxSituationOptions(licenceType)
        taxSituationForm(options)
          .bindFromRequest()
          .fold(
            formWithErrors => {
              val (startDate, endDate) = getTaxPeriodStrings(taxYear)
              Ok(
                taxSituationPage(
                  formWithErrors,
                  journeyService.previous(routes.TaxSituationController.taxSituation()),
                  options,
                  startDate,
                  endDate
                )
              )
            },
            handleValidTaxSituation
          )
      }

    }
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

  val saTaxSituations: Seq[TaxSituation] = Seq(TaxSituation.SA, TaxSituation.SAPAYE)

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

  def getTaxPeriodStrings(taxYear: TaxYear)(implicit messages: Messages): (String, String) = {
    val start = LocalDate.of(taxYear.startYear, 4, 6)
    val end   = LocalDate.of(taxYear.startYear + 1, 4, 5)
    TimeUtils.govDisplayFormat(start) -> TimeUtils.govDisplayFormat(end)
  }

}
