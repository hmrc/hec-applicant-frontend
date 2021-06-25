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
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthAction
import uk.gov.hmrc.hecapplicantfrontend.services.IvService
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.hecapplicantfrontend.util.Logging.LoggerOps
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import java.util.UUID
import scala.concurrent.ExecutionContext

@Singleton
class IvFailureController @Inject() (
  authAction: AuthAction,
  ivService: IvService,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with Logging {

  def ivFailure(journeyId: UUID): Action[AnyContent] = authAction.async { implicit request =>
    ivService
      .getFailedJourneyStatus(journeyId)
      .fold(
        { e =>
          logger.warn("Could not check IV journey error status", e)
          InternalServerError
        },
        ivErrorStatus => Ok(s"Got IV error status $ivErrorStatus")
      )
  }

}
