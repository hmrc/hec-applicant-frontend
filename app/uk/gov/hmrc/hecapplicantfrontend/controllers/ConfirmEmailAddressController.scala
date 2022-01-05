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
import uk.gov.hmrc.hecapplicantfrontend.services.EmailVerificationService
import cats.instances.future._
import com.google.inject.{Inject, Singleton}
import play.api.data.{Form, Mapping}
import uk.gov.hmrc.emailaddress.{EmailAddress => EmailAddressValidation}
import play.api.i18n.I18nSupport
import play.api.mvc._
import uk.gov.hmrc.hecapplicantfrontend.controllers.ConfirmEmailAddressController.{emailAddressForm, emailTypeOptions}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, UserEmailAnswers, UserSelectedEmail}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController
import uk.gov.voa.play.form.ConditionalMappings.mandatoryIfEqual

import java.util.Locale
import scala.concurrent.{ExecutionContext, Future}

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

  val confirmEmailAddress: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.ensureGGEmailIdPresent { ggEmail =>
      val userEmailAnswerOpt: Option[UserEmailAnswers] = request.sessionData.userEmailAnswers
      val back                                         = journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress)

      val form = {
        val emptyForm = emailAddressForm(emailTypeOptions, ggEmail)
        userEmailAnswerOpt.fold(emptyForm)(userEmail =>
          emptyForm
            .fill(UserSelectedEmail(userEmail.userSelectedEmail.emailType, userEmail.userSelectedEmail.emailAddress))
        )
      }
      Ok(confirmEmailAddressPage(form, back, emailTypeOptions, ggEmail.value))
    }
  }

  val confirmEmailAddressSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    request.sessionData.ensureGGEmailIdPresent { ggEmail =>
      def handleValidEmail(userSelectedEmail: UserSelectedEmail): Future[Result] = {
        //val authReq: AuthenticatedRequest[AnyContent] = request.request
//        val headerCarrier: HeaderCarrier              =
//          HeaderCarrierConverter.fromRequestAndSession(request, request.session)

        val result = for {
          passcodeResult     <-
            emailVerificationService.requestPasscode(userSelectedEmail.emailAddress)
          updatedEmailAnswers =
            Some(UserEmailAnswers(userSelectedEmail, passcodeResult.some, None, None, None))
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

      emailAddressForm(emailTypeOptions, ggEmail)
        .bindFromRequest()
        .fold(
          formWithErrors =>
            Ok(
              confirmEmailAddressPage(
                formWithErrors,
                journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress()),
                emailTypeOptions,
                ggEmail.value
              )
            ),
          handleValidEmail
        )
    }
  }

}

object ConfirmEmailAddressController {

  import play.api.data.validation.{Constraint, Invalid, Valid}
  import play.api.data.Forms.{mapping, nonEmptyText, of}

  val emailTypeOptions: List[EmailType] = List(EmailType.GGEmail, EmailType.DifferentEmail)

  val differentEmailAddressMapping: Mapping[EmailAddress] = nonEmptyText
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

  def emailAddressForm(options: List[EmailType], ggEmailId: EmailAddress): Form[UserSelectedEmail] = Form(
    mapping(
      "confirmEmailAddress" -> of(FormUtils.radioFormFormatter(options)),
      "differentEmail"      -> mandatoryIfEqual("confirmEmailAddress", "1", differentEmailAddressMapping)
    ) { (_, optionalEmail) =>
      optionalEmail match {
        case Some(email) => UserSelectedEmail(EmailType.DifferentEmail, email)
        case _           => UserSelectedEmail(EmailType.GGEmail, ggEmailId)
      }
    } { ue =>
      ue.emailType match {
        case EmailType.GGEmail        => (EmailType.GGEmail, ggEmailId.some).some
        case EmailType.DifferentEmail => (EmailType.DifferentEmail, ue.emailAddress.some).some
      }
    }
  )
}
