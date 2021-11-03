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

package uk.gov.hmrc.hecapplicantfrontend.testonly.controllers

import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.{routes => nonTestRoutes}
import uk.gov.hmrc.hecapplicantfrontend.models.EmailAddress
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, NINO}
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.LoginData
import uk.gov.hmrc.hecapplicantfrontend.testonly.services.AuthLoginStubService
import uk.gov.hmrc.hecapplicantfrontend.testonly.views.html
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

@Singleton
class JourneyStarterController @Inject() (
  journeyStartPage: html.JourneyStarter,
  authLoginStubService: AuthLoginStubService,
  appConfig: AppConfig,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  private lazy val redirectUrl = s"${appConfig.selfBaseUrl}${nonTestRoutes.StartController.start().url}"

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val journeyStarter: Action[AnyContent] = Action { implicit request =>
    Ok(journeyStartPage())
  }

  val startJourney1: Action[AnyContent] = Action.async { _ =>
    handleLogin(
      LoginData(
        redirectUrl,
        ConfidenceLevel.L250,
        AffinityGroup.Individual,
        EmailAddress("user@test.com"),
        Some(NINO("AB123456C")),
        None
      )
    )
  }

  val startJourney2: Action[AnyContent] = Action.async { _ =>
    handleLogin(
      LoginData(
        redirectUrl,
        ConfidenceLevel.L50,
        AffinityGroup.Organisation,
        EmailAddress("user@test.com"),
        None,
        Some(ctEnrolment(CTUTR("1111111111")))
      )
    )
  }

  private def handleLogin(loginData: LoginData) =
    authLoginStubService
      .login(loginData)
      .fold(
        e => sys.error(s"Could not login: $e"),
        SeeOther(redirectUrl).withSession
      )

  private def ctEnrolment(ctutr: CTUTR): Enrolment =
    Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", ctutr.value)), "Activated")

}
