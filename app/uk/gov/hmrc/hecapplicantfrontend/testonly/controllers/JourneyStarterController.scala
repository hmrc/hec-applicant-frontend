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

package uk.gov.hmrc.hecapplicantfrontend.testonly.controllers

import cats.data.EitherT
import cats.instances.future.*
import cats.instances.list.*
import cats.syntax.traverse.*
import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.{routes => nonTestOnlyRoutes}
import uk.gov.hmrc.hecapplicantfrontend.controllers.toFuture
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.testonly.controllers.JourneyStarterController.*
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.Journey.*
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.Journey
import uk.gov.hmrc.hecapplicantfrontend.testonly.services.{AuthLoginStubService, HECService, JourneyToLoginDataTransformer}
import uk.gov.hmrc.hecapplicantfrontend.testonly.views.html
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class JourneyStarterController @Inject() (
  journeyStartPage: html.JourneyStarter,
  authLoginStubService: AuthLoginStubService,
  hecService: HECService,
  journeyToLoginDataTransformer: JourneyToLoginDataTransformer,
  appConfig: AppConfig,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private val redirectUrl = s"${appConfig.selfBaseUrl}${nonTestOnlyRoutes.StartController.start.url}"

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val journeyStarter: Action[AnyContent] = Action { implicit request =>
    Ok(journeyStartPage(journeyForm, journeyOptions))
  }

  val journeyStarterSubmit: Action[AnyContent] = Action.async { implicit request =>
    journeyForm
      .bindFromRequest()
      .fold(
        formWithErrors => Ok(journeyStartPage(formWithErrors, journeyOptions)),
        journey => loginAndRedirect(journey)
      )
  }

  private def loginAndRedirect(journey: Journey): Future[Result] = {
    val loginData = journeyToLoginDataTransformer.toLoginData(journey, redirectUrl)
    val result    = for {
      _       <- if (loginData.existingTaxChecks.nonEmpty)
                   loginData.existingTaxChecks
                     .map(hecService.saveTaxCheck)
                     .sequence[[A] =>> EitherT[Future, Error, A], Unit]
                 else
                   EitherT.pure[Future, Error](List.empty)
      session <- authLoginStubService.login(loginData)
    } yield session

    result
      .fold(
        e => sys.error(s"Could not login: $e"),
        SeeOther(loginData.redirectUrl).withSession
      )
  }

}

object JourneyStarterController {

  val journeyOptions: List[Journey] = List(
    IndividualNoSA,
    IndividualNoSANoGGEmail,
    IndividualSAReturnFound,
    IndividualSANoticeToFileIssued,
    IndividualSANoReturnFound,
    IndividualSAReturnFoundExistingTaxCheck,
    CompanyNoCTEnrolment,
    CompanyCTReturnFound,
    CompanyCTNoticeToFileIssued,
    CompanyCTNoReturnFound,
    CompanyCTNoAccountingPeriods
  )

  val journeyForm: Form[Journey] =
    Form(
      mapping(
        "journey" -> of(FormUtils.radioFormFormatter(journeyOptions))
      )(identity)(Some(_))
    )

}
