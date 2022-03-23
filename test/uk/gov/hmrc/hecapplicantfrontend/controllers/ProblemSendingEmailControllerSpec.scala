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
import uk.gov.hmrc.hecapplicantfrontend.models.EmailAddress
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

class ProblemSendingEmailControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
  )

  val controller = instanceOf[ProblemSendingEmailController]

  "ProblemSendingEmailControllerSpec" when {

    "handling request to problem sending email page" must {

      val ggEmailId                       = EmailAddress("user@test.com")
      val expiredPasscode                 = Passcode("FFFFFF")
      def performAction(): Future[Result] = controller.problemSendingEmail(FakeRequest())

      def getSession(emailSendResult: Option[EmailSendResult]) = Fixtures.individualHECSession(
        loginData = Fixtures.individualLoginData(emailAddress = ggEmailId.some),
        emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
        userEmailAnswers = Fixtures
          .userEmailAnswers(
            passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
            passcode = expiredPasscode.some,
            passcodeVerificationResult = PasscodeVerificationResult.Match.some,
            emailSendResult = emailSendResult
          )
          .some
      )

      "return a technical error" when {

        "Email send result result is other than Failure" in {
          val session = getSession(EmailSendResult.EmailSent.some)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))

        }

      }

      "display the page" in {
        val session = getSession(EmailSendResult.EmailSentFailure.some)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(
            routes.ProblemSendingEmailController.problemSendingEmail,
            session
          )(
            mockPreviousCall
          )
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("problemSendingEmail.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val htmlBody = doc.select(".govuk-body").html()

            htmlBody should include regex messageFromMessageKey(
              "problemSendingEmail.p2",
              routes.StartController.start.url
            )

          }
        )

      }

    }

  }

}
