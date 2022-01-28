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

import cats.implicits.catsSyntaxOptionId
import com.google.inject.Inject
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailParameters
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, SendEmailService}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging, TimeUtils}
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.hecapplicantfrontend.views.html

import scala.concurrent.ExecutionContext

class EmailAddressConfirmedController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  sendEmailService: SendEmailService,
  emailAddressConfirmedPage: html.EmailAddressConfirmed,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val emailAddressConfirmed: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    request.sessionData.ensureUserSelectedEmailPresent { userSelectedEmail =>
      request.sessionData.verifyPasscodeVerificationResultAndPasscodeRequestResult {
        val previous = journeyService.previous(routes.EmailAddressConfirmedController.emailAddressConfirmed())
        Ok(emailAddressConfirmedPage(userSelectedEmail.emailAddress, previous))
      }
    }
  }

  val emailAddressConfirmedSubmit: Action[AnyContent] =
    authAction.andThen(sessionDataAction).async { implicit request =>
      request.sessionData.ensureUserSelectedEmailPresent { userSelectedEmail =>
        request.sessionData.verifyPasscodeVerificationResultAndPasscodeRequestResult {
          val existingUserEmailAnswers = request.sessionData.userEmailAnswers
          val emailParameters          = getEmailParameters(request.sessionData)

          val result = for {
            result             <-
              sendEmailService.sendEmail(userSelectedEmail, emailParameters)
            updatedEmailAnswers = existingUserEmailAnswers.map(_.copy(emailSendResult = result.some))
            updatedSession      =
              request.sessionData
                .fold(_.copy(userEmailAnswers = updatedEmailAnswers), _.copy(userEmailAnswers = updatedEmailAnswers))
            next               <-
              journeyService
                .updateAndNext(routes.EmailAddressConfirmedController.emailAddressConfirmed(), updatedSession)
          } yield next

          result.fold(
            _.doThrow("Could not update session and proceed"),
            Redirect
          )
        }
      }
    }

  private def getEmailParameters(session: HECSession)(implicit request: RequestWithSessionData[_]) = {
    val hecTaxCheckCode        = session.completedTaxCheck
      .map(_.taxCheckCode)
      .getOrElse(sys.error(" Tax check code is not in session"))
    val taxCheckCodeExpiryDate = session.completedTaxCheck
      .map(_.expiresAfter)
      .getOrElse(sys.error(" Tax check code expiry date is not in session"))

    val taxCheckCodeCreatedDate = session.completedTaxCheck
      .map(_.createDate)
      .getOrElse(
        sys.error(" Tax check code created date is not in session")
      )

    val licenceType: LicenceType = session.userAnswers
      .fold(_.fold(_.licenceType, _.licenceType.some), _.fold(_.licenceType, _.licenceType.some))
      .getOrElse(sys.error("Licence Type is not in session"))

    EmailParameters(
      currentDate = s"${TimeUtils.govDisplayFormat(taxCheckCodeCreatedDate.toLocalDate)}",
      licenceType = s"${FormUtils.licenceTypeFormat(licenceType)}",
      hecTaxCheckCode = s"${hecTaxCheckCode.value.removeWhitespace.grouped(3).mkString(" ")}",
      expiresAfter = s"${TimeUtils.govDisplayFormat(taxCheckCodeExpiryDate)}"
    )

  }
}
