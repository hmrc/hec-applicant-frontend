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

import com.google.inject.Inject
import play.api.data.Form
import play.api.data.Forms.{mapping, of}
import play.api.i18n.I18nSupport
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthAction, SessionDataAction}
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyNameConfirmed, RetrievedApplicantData}
import uk.gov.hmrc.hecapplicantfrontend.models.views.CompanyNameConfirmedOption
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.{FormUtils, Logging}
import uk.gov.hmrc.hecapplicantfrontend.views.html
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

class CompanyDetailsController @Inject() (
  authAction: AuthAction,
  sessionDataAction: SessionDataAction,
  journeyService: JourneyService,
  confirmCompanyNamePage: html.ConfirmCompanyName,
  mcc: MessagesControllerComponents
) extends FrontendController(mcc)
    with I18nSupport
    with Logging {

  val confirmCompanyDetails: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    val back = journeyService.previous(routes.CompanyDetailsController.confirmCompanyDetails())
    request.sessionData.retrievedUserData match {
      case RetrievedApplicantData.CompanyRetrievedData(_, _, _, Some(companyHouseName), _) =>
        val companyNameConfirmed = request.sessionData.userAnswers.fold(_.companyNameConfirmed, _.companyNameConfirmed)
        val form = {
          val emptyForm = CompanyDetailsController.confirmCompanyNameForm(CompanyNameConfirmed.values)
          companyNameConfirmed.fold(emptyForm)(emptyForm.fill)
        }
        Ok(
          confirmCompanyNamePage(
            form,
            back,
            companyHouseName.name,
            CompanyDetailsController.companyNameConfirmedOptions
          )
        )
      case RetrievedApplicantData.CompanyRetrievedData(_, _, _, None, _)                   =>
        logger.warn("Missing company name")
        InternalServerError
      case _: RetrievedApplicantData.IndividualRetrievedData                               =>
        logger.warn("Confirm company name called for individual applicant")
        InternalServerError
    }
  }

  val confirmCompanyDetailsSubmit: Action[AnyContent] = authAction.andThen(sessionDataAction) { implicit request =>
    Ok(
      s" The session is ${request.sessionData} and back Url is : ${journeyService
        .previous(routes.CompanyDetailsController.confirmCompanyDetails())}"
    )
  }
}

object CompanyDetailsController {
  val companyNameConfirmedOptions: List[CompanyNameConfirmedOption] =
    CompanyNameConfirmed.values.map(CompanyNameConfirmedOption.companyNameConfirmedToOption)

  def confirmCompanyNameForm(options: List[CompanyNameConfirmed]): Form[CompanyNameConfirmed] =
    Form(
      mapping(
        "confirmCompanyName" -> of(FormUtils.radioFormFormatter(options))
      )(identity)(Some(_))
    )
}
