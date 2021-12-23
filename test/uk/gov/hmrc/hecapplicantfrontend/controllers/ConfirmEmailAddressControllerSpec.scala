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
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, EmailType, UserEmailAnswers}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{EmailVerificationService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class ConfirmEmailAddressControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockEmailVerificationService = mock[EmailVerificationService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[EmailVerificationService].toInstance(mockEmailVerificationService)
  )

  val controller = instanceOf[ConfirmEmailAddressController]

  "ConfirmEmailAddressControllerSpec" when {
    val otherEmailId = EmailAddress("user1@test.com")

    "handling request to confirm email address page" must {
      def performAction(): Future[Result] = controller.confirmEmailAddress(FakeRequest())

      "return a technical error" when {

        "GG account email id is not in session" in {
          val session = Fixtures.individualHECSession(
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "display the page" when {

        def test(userEmailAnswers: Option[UserEmailAnswers], selected: Option[String]) = {
          val session = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = EmailAddress("user@test.com").some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = userEmailAnswers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.ConfirmEmailAddressController.confirmEmailAddress(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmEmailAddress.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selected match {
                case Some(value) => selectedOptions.attr("value") shouldBe value
                case None        => selectedOptions.isEmpty       shouldBe true
              }
              doc.select("#differentEmail").attr("value") shouldBe userEmailAnswers
                .flatMap(_.emailAddress.map(_.value))
                .getOrElse("")

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.ConfirmEmailAddressController.confirmEmailAddressSubmit().url
            }
          )
        }

        "User's GG account has a valid email id and user hasn't previously answered the questions " in {
          test(userEmailAnswers = None, selected = None)
        }

        "User's GG account has a valid email id and user has previously selected ggEmail id  " in {
          test(UserEmailAnswers(EmailType.GGEmail, None, None, None).some, Some("0"))
        }

        "User's GG account has a valid email id and user has previously selected different email id option" in {
          test(UserEmailAnswers(EmailType.DifferentEmail, otherEmailId.some, None, None).some, Some("1"))

        }
      }
    }

//    "handling submit to confimr email address page" must {
//
//      def performAction(data: (String, String)*): Future[Result] =
//        controller.confirmEmailAddressSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))
//
//      behave like authAndSessionDataBehaviour(() => performAction())
//
//      "show a form error" when {
//
//        val session = Fixtures.individualHECSession(
//          userAnswers = Fixtures.completeIndividualUserAnswers(),
//          isEmailRequested = true,
//          userEmailAnswers = UserEmailAnswers().some
//        )
//
//        "nothing is submitted" in {
//          inSequence {
//            mockAuthWithNoRetrievals()
//            mockGetSession(session)
//            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
//              mockPreviousCall
//            )
//          }
//
//          checkFormErrorIsDisplayed(
//            performAction(),
//            messageFromMessageKey("licenceTimeTrading.title"),
//            messageFromMessageKey("licenceTimeTrading.error.required")
//          )
//        }
//
//        "an index is submitted which is too large" in {
//          inSequence {
//            mockAuthWithNoRetrievals()
//            mockGetSession(session)
//            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
//              mockPreviousCall
//            )
//          }
//
//          checkFormErrorIsDisplayed(
//            performAction("licenceTimeTrading" -> Int.MaxValue.toString),
//            messageFromMessageKey("licenceTimeTrading.title"),
//            messageFromMessageKey("licenceTimeTrading.error.invalid")
//          )
//        }
//
//        "a value is submitted which is not a number" in {
//          inSequence {
//            mockAuthWithNoRetrievals()
//            mockGetSession(session)
//            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
//              mockPreviousCall
//            )
//          }
//
//          checkFormErrorIsDisplayed(
//            performAction("licenceTimeTrading" -> "xyz"),
//            messageFromMessageKey("licenceTimeTrading.title"),
//            messageFromMessageKey("licenceTimeTrading.error.invalid")
//          )
//        }
//
//      }
//
//    }
  }

}
