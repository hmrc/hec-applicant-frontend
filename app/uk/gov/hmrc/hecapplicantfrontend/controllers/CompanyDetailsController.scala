package uk.gov.hmrc.hecapplicantfrontend.controllers

import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.MessagesControllerComponents
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.ExecutionContext

class CompanyDetailsController @Inject() (
                                           authAction: AuthAction,
                                           sessionDataAction: SessionDataAction,
                                           journeyService: JourneyService,
                                           mcc: MessagesControllerComponents
                                         )(implicit ec: ExecutionContext, appConfig: AppConfig)
  extends FrontendController(mcc)
    with I18nSupport
    with Logging {
  val

    }
