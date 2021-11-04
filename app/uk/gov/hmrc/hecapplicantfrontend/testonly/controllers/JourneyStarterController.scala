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

import cats.data.EitherT
import cats.instances.future._
import cats.instances.list._
import cats.syntax.traverse._
import com.google.inject.{Inject, Singleton}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Result}
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.{routes => nonTestRoutes}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, Error, HECTaxCheckCode, HECTaxCheckSource}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.{LoginData, SaveTaxCheckRequest}
import uk.gov.hmrc.hecapplicantfrontend.testonly.services.{AuthLoginStubService, HECService}
import uk.gov.hmrc.hecapplicantfrontend.testonly.views.html
import uk.gov.hmrc.hecapplicantfrontend.util.{Logging, TimeUtils}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.time.LocalDate
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class JourneyStarterController @Inject() (
  journeyStartPage: html.JourneyStarter,
  authLoginStubService: AuthLoginStubService,
  hecService: HECService,
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

  val individualJourney: Action[AnyContent] = Action.async { _ =>
    handleLogin(
      LoginData(
        newGGCredId(),
        redirectUrl,
        ConfidenceLevel.L250,
        AffinityGroup.Individual,
        EmailAddress("user@test.com"),
        Some(NINO("AB123456C")),
        None,
        List.empty
      )
    )
  }

  val individualJourneyWithExistingTaxChecks: Action[AnyContent] = Action.async { _ =>
    val ggCredId         = newGGCredId()
    val existingTaxCheck = SaveTaxCheckRequest(
      HECTaxCheckCode("A2345K7T9"),
      ggCredId,
      LicenceType.DriverOfTaxisAndPrivateHires,
      Right(DateOfBirth(LocalDate.of(2000, 1, 1))),
      TimeUtils.today().plusDays(10L),
      TimeUtils.now(),
      TimeUtils.now(),
      isExtracted = true,
      HECTaxCheckSource.Digital
    )
    handleLogin(
      LoginData(
        ggCredId,
        redirectUrl,
        ConfidenceLevel.L250,
        AffinityGroup.Individual,
        EmailAddress("user@test.com"),
        Some(NINO("AB123456C")),
        None,
        List(existingTaxCheck)
      )
    )
  }

  val companyJourneyReturnFound: Action[AnyContent] = Action.async { _ =>
    handleLogin(
      LoginData(
        newGGCredId(),
        redirectUrl,
        ConfidenceLevel.L50,
        AffinityGroup.Organisation,
        EmailAddress("user@test.com"),
        None,
        Some(ctEnrolment(CTUTR("1111111111"))),
        List.empty
      )
    )
  }

  private def newGGCredId(): GGCredId =
    GGCredId(UUID.randomUUID.toString)

  private def handleLogin(loginData: LoginData): Future[Result] = {
    val result = for {
      _       <- if (loginData.existingTaxChecks.nonEmpty)
                   loginData.existingTaxChecks
                     .map(hecService.saveTaxCheck)
                     .sequence[EitherT[Future, Error, *], Unit]
                 else
                   EitherT.pure[Future, Error](List.empty)
      session <- authLoginStubService.login(loginData)
    } yield session

    result
      .fold(
        e => sys.error(s"Could not login: $e"),
        SeeOther(redirectUrl).withSession
      )
  }

  private def ctEnrolment(ctutr: CTUTR): Enrolment =
    Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", ctutr.value)), "Activated")

}
