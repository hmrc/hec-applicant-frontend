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

import cats.implicits.catsSyntaxOptionId
import uk.gov.hmrc.hecapplicantfrontend.services.EmailVerificationService
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.data.{Form, Mapping}
import uk.gov.hmrc.emailaddress.{EmailAddress => EmailAddressValidation}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.controllers.ConfirmEmailAddressController.{emailAddressForm, getEmailOptions}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, AuthenticatedRequest, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, UserEmail, UserEmailAnswers}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.hmrc.play.http.HeaderCarrierConverter
import uk.gov.voa.play.form.ConditionalMappings.mandatoryIfEqual

import java.util.Locale
import scala.concurrent.ExecutionContext

@Singleton
class ConfirmEmailAddressController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  emailVerificationService: EmailVerificationService,
  confirmEmailAddressPage: html.ConfirmEmailAddress,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmEmailAddress: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val ggEmail                                      = request.sessionData
      .fold(_.loginData.emailAddress, _.loginData.emailAddress)
      .getOrElse(sys.error(" Email Address is not present in session"))
    val emailOptions                                 = getEmailOptions
    val userEmailAnswerOpt: Option[UserEmailAnswers] = request.sessionData.userEmailAnswers
    val back                                         = journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress)

    val form = {
      val emptyForm = emailAddressForm(emailOptions)
      userEmailAnswerOpt.fold(emptyForm)(i => emptyForm.fill(UserEmail(i.emailType, i.emailAddress)))
    }
    Ok(confirmEmailAddressPage(form, back, emailOptions, ggEmail.value))
  }

  val confirmEmailAddressSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    val ggEmail      = request.sessionData
      .fold(_.loginData.emailAddress, _.loginData.emailAddress)
      .getOrElse(sys.error("No  Email Address found in GG account"))
    val emailOptions = getEmailOptions

    def handleValidEmail(userEmail: UserEmail) = {
      val userSelectedEmail                         = userEmail.emailAddress.getOrElse(ggEmail)
      val authReq: AuthenticatedRequest[AnyContent] = request.request
      val headerCarrier: HeaderCarrier              =
        HeaderCarrierConverter.fromRequestAndSession(request, request.session)

      val result = for {
        passcodeResult     <- emailVerificationService.requestPasscode(userSelectedEmail)(headerCarrier, authReq)
        updatedEmailAnswers =
          Some(UserEmailAnswers(userEmail.emailType, userEmail.emailAddress, passcodeResult.some, None))
        updatedSession      =
          request.sessionData
            .fold(_.copy(userEmailAnswers = updatedEmailAnswers), _.copy(userEmailAnswers = updatedEmailAnswers))
        next               <-
          journeyService.updateAndNext(routes.ConfirmEmailAddressController.confirmEmailAddress(), updatedSession)
      } yield next

      result.fold(
        _.doThrow("Could not update session and proceed"),
        Redirect
      )
    }

    emailAddressForm(emailOptions)
      .bindFromRequest()
      .fold(
        formWithErrors =>
          Ok(
            confirmEmailAddressPage(
              formWithErrors,
              journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress()),
              emailOptions,
              ggEmail.value
            )
          ),
        handleValidEmail
      )

  }

}

object ConfirmEmailAddressController {

  import play.api.data.validation.{Constraint, Invalid, Valid}
  import play.api.data.Forms.{mapping, nonEmptyText, of}

  val getEmailOptions: List[EmailType] = List(EmailType.GGEmail, EmailType.DifferentEmail)

  def differentEmailAddressMapping: Mapping[EmailAddress] = nonEmptyText
    .transform[EmailAddress](
      email => EmailAddress(email.toLowerCase(Locale.UK)),
      _.value
    )
    .verifying(
      Constraint[EmailAddress]((email: EmailAddress) =>
        if (email.value.length > 256) Invalid("error.tooManyChar")
        else if (EmailAddressValidation.isValid(email.value)) Valid
        else Invalid("error.invalidFormat")
      )
    )

  def emailAddressForm(options: List[EmailType]): Form[UserEmail] = Form(
    mapping(
      "confirmEmailAddress" -> of(FormUtils.radioFormFormatter(options)),
      "differentEmail"      -> mandatoryIfEqual("confirmEmailAddress", "1", differentEmailAddressMapping)
    ) { (_, optionalEmail) =>
      optionalEmail match {
        case Some(email) => UserEmail(EmailType.DifferentEmail, email.some)
        case _           => UserEmail(EmailType.GGEmail, None)
      }
    } { ue =>
      ue.emailType match {
        case EmailType.GGEmail        => (EmailType.GGEmail, None).some
        case EmailType.DifferentEmail => (EmailType.DifferentEmail, ue.emailAddress).some
      }
    }
  )
}
