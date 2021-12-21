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

import com.google.inject.{Inject, Singleton}
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.ConfirmEmailAddressController.{emailAddressForm, getEmailOptions}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.EmailAddress
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

@Singleton
class ConfirmEmailAddressController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  confirmEmailAddressPage: html.ConfirmEmailAddress,
  mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmEmailAddress: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back         = journeyService.previous(routes.ConfirmEmailAddressController.confirmEmailAddress)
    val emailOpt     = request.sessionData.fold(_.loginData.emailAddress, _.loginData.emailAddress)
    val emailOptions = getEmailOptions(emailOpt)

    val form: Form[EmailAddress] = {
      val emptyForm: Form[EmailAddress] = emailAddressForm(emailOptions)
      emailOpt.fold(emptyForm)(emptyForm.fill)
    }
    Ok(confirmEmailAddressPage(form, back, emailOptions))
  }

  val confirmEmailAddressSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction).async { implicit request =>
    Ok(s"${request.sessionData}")
  }

}

object ConfirmEmailAddressController {

  def getEmailOptions(emailAddress: Option[EmailAddress]): List[EmailAddress] = emailAddress match {
    case Some(email) => List(email, EmailAddress("different"))
    case None        => sys.error(" Email address is not present in user's login data.")
  }

  def emailAddressForm(options: List[EmailAddress]): Form[EmailAddress] = Form(
    mapping(
      "confirmEmailAddress" -> of(FormUtils.radioFormFormatter(options))
    )(identity)(Some(_))
  )
}
