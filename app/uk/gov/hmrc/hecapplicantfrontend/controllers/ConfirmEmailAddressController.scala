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
import play.api.data.{Form, Mapping}
import uk.gov.hmrc.emailaddress.{EmailAddress => EmailAddressValidation}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.controllers.ConfirmEmailAddressController.{emailAddressForm, getEmailOptions}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, RequestWithSessionData, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{ConfirmUserEmail, EmailAddress, HECSession, UserEmailAnswers}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.voa.play.form.ConditionalMappings.{mandatoryIfEqual}

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class ConfirmEmailAddressController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  confirmEmailAddressPage: html.ConfirmEmailAddress,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmEmailAddress: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back               = journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress)
    val ggEmailOpt         = request.sessionData.fold(_.loginData.emailAddress, _.loginData.emailAddress)
    val userEmailAnswerOpt = request.sessionData.userEmailAnswers.map(_.emailAddress)
    val emailOptions       = getEmailOptions(ggEmailOpt.getOrElse(sys.error(" Email Address is not present in session")))

    val form = {
      val emptyForm = emailAddressForm(emailOptions)
      userEmailAnswerOpt.fold(emptyForm)(email => emptyForm.fill(ConfirmUserEmail(email, None)))
    }
    Ok(confirmEmailAddressPage(form, back, emailOptions))
  }

  val confirmEmailAddressSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val ggEmail      = request.sessionData
      .fold(_.loginData.emailAddress, _.loginData.emailAddress)
      .getOrElse(sys.error("No  Email Address found in GG account"))
    val emailOptions = getEmailOptions(ggEmail)

    def handleValidEmail(confirmUserEmail: ConfirmUserEmail) = {
      val emailAns: Option[UserEmailAnswers] = request.sessionData.fold(_.userEmailAnswers, _.userEmailAnswers)
      val updatedEmailAnswers                = confirmUserEmail.differentEmail match {
        case Some(email) => emailAns.map(_.copy(emailAddress = email))
        case None        => emailAns.map(_.copy(emailAddress = confirmUserEmail.ggEmail))
      }
      val updatedSession                     = request.sessionData
        .fold(_.copy(userEmailAnswers = updatedEmailAnswers), _.copy(userEmailAnswers = updatedEmailAnswers))

      updateAndNextJourneyData(routes.ConfirmEmailAddressController.confirmEmailAddress(), updatedSession)
    }

    emailAddressForm(emailOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            confirmEmailAddressPage(
              formWithErrors,
              journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress()),
              emailOptions
            )
          ),
        handleValidEmail
      )

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

object ConfirmEmailAddressController {

  import play.api.data.validation.{Constraint, Invalid, Valid}
  import play.api.data.Forms.{mapping, nonEmptyText, of}

  def getEmailOptions(emailAddress: EmailAddress): List[EmailAddress] = List(emailAddress, EmailAddress("different"))

  def differentEmailAddressMapping: Mapping[EmailAddress] = nonEmptyText
    .transform[EmailAddress](
      email => EmailAddress(email.toLowerCase(Locale.UK)),
      _.value
    )
    .verifying(
      Constraint[EmailAddress]((email: EmailAddress) =>
        if (EmailAddressValidation.isValid(email.value)) Valid
        else Invalid("error.invalidFormat")
      )
    )

  def emailAddressForm(options: List[EmailAddress]): Form[ConfirmUserEmail] = Form(
    mapping(
      "confirmEmailAddress" -> of(FormUtils.radioFormFormatter(options)),
      "differentEmail"      -> mandatoryIfEqual("confirmEmailAddress", "1", differentEmailAddressMapping)
    )((ggEmail, optionalEmail) => ConfirmUserEmail(ggEmail, optionalEmail))(i => Some((i.ggEmail, i.differentEmail)))
  )
}
