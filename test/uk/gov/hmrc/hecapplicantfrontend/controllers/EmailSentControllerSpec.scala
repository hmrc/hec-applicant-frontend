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
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class EmailSentControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore)
  )

  val controller      = instanceOf[EmailSentController]
  val ggEmailId       = EmailAddress("user@test.com")
  val userEmailAnswer = Fixtures
    .userEmailAnswers(
      passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
      passcode = Passcode("HHHHHH").some,
      passcodeVerificationResult = PasscodeVerificationResult.Match.some
    )

  "EmailSentControllerSpec" when {

    "handling request to email sent page" must {
      def performAction(): Future[Result] = controller.emailSent(FakeRequest())

      "return a technical error page" when {

        "user selected email is not in session" in {

          val session = Fixtures.individualHECSession(
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            isEmailRequested = true,
            userEmailAnswers = None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "display the page" in {
        val session: HECSession = Fixtures.companyHECSession(
          loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
          userAnswers = Fixtures.completeCompanyUserAnswers(),
          isEmailRequested = true,
          userEmailAnswers = userEmailAnswer.some
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("emailSent.title"),
          { doc =>
            doc.select(".govuk-inset-text").text() should include regex "user@test.com"
            doc.select(".govuk-body").html()       should include regex messageFromMessageKey(
              "emailSent.p3",
              routes.StartController.start().url
            )

          }
        )
      }
    }
  }
}
