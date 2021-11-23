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

package uk.gov.hmrc.hecapplicantfrontend.testonly.controllers

import cats.data.EitherT
import cats.instances.future._
import play.api.inject.bind
import play.api.mvc.{Result, Session}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.ConfidenceLevel.L250
import uk.gov.hmrc.hecapplicantfrontend.controllers.{routes => nonTestOnlyRoutes}
import uk.gov.hmrc.hecapplicantfrontend.controllers.ControllerSpec
import uk.gov.hmrc.hecapplicantfrontend.models.HECTaxCheckSource.Digital
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, Error, HECTaxCheckCode}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.GGCredId
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.Journey._
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.{Journey, LoginData, SaveTaxCheckRequest}
import uk.gov.hmrc.hecapplicantfrontend.testonly.services.{AuthLoginStubService, HECService, JourneyToLoginDataTransformer}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class JourneyStarterControllerSpec extends ControllerSpec {

  val mockJourneyToLoginDataTransformer = mock[JourneyToLoginDataTransformer]

  val mockAuthLoginStubService = mock[AuthLoginStubService]

  val mockHECService = mock[HECService]

  override def overrideBindings = List(
    bind[JourneyToLoginDataTransformer].toInstance(mockJourneyToLoginDataTransformer),
    bind[AuthLoginStubService].toInstance(mockAuthLoginStubService),
    bind[HECService].toInstance(mockHECService)
  )

  def mockTransformJourneyToLoginData(journey: Journey, redirectUrl: String)(result: LoginData) =
    (mockJourneyToLoginDataTransformer
      .toLoginData(_: Journey, _: String))
      .expects(journey, redirectUrl)
      .returning(result)

  def mockLogin(loginData: LoginData)(result: Either[Error, Session]) =
    (mockAuthLoginStubService
      .login(_: LoginData)(_: HeaderCarrier))
      .expects(loginData, *)
      .returning(EitherT.fromEither(result))

  def mockSaveTaxCheck(saveTaxCheckRequest: SaveTaxCheckRequest)(result: Either[Error, Unit]) =
    (mockHECService
      .saveTaxCheck(_: SaveTaxCheckRequest)(_: HeaderCarrier))
      .expects(saveTaxCheckRequest, *)
      .returning(EitherT.fromEither(result))

  val controller = instanceOf[JourneyStarterController]

  "JourneyStarterController" when {

    "handling requests to display the start journey page" must {

      "display the page" in {
        checkPageIsDisplayed(
          controller.journeyStarter(FakeRequest()),
          "Start a journey",
          { doc =>
            testRadioButtonOptions(
              doc,
              List(
                "Individual, no SA UTR",
                "Individual, SA return found",
                "Individual, SA notice to file issued (but return not found)",
                "Individual, SA no return found (and no notice to file issued)",
                "Individual, SA return found with existing tax check",
                "Company, no CT enrolment",
                "Company, CT return found",
                "Company, CT notice to file issued (but return not found)",
                "Company, CT no return found (and no notice to file issued)",
                "Company, CT no accounting period found"
              )
            )

            doc.select("form").attr("action") shouldBe routes.JourneyStarterController.journeyStarterSubmit().url
          }
        )

      }

    }

    "handling submits on the start journey page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.journeyStarterSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      val expectedRedirectUrl = s"${appConfig.selfBaseUrl}${nonTestOnlyRoutes.StartController.start().url}"

      val loginDataNoExistingTaxChecks =
        LoginData(
          GGCredId(""),
          "redirect",
          L250,
          Individual,
          EmailAddress(""),
          None,
          None,
          List.empty
        )

      val existingTaxCheck =
        SaveTaxCheckRequest(
          HECTaxCheckCode("code"),
          GGCredId("id"),
          LicenceType.OperatorOfPrivateHireVehicles,
          Right(DateOfBirth(LocalDate.now())),
          LocalDate.now(),
          ZonedDateTime.now(),
          ZonedDateTime.now(),
          isExtracted = true,
          Digital
        )

      val session = Session(Map("key" -> "value"))

      "return a form error" when {

        "nothing is submitted" in {
          checkFormErrorIsDisplayed(
            performAction(),
            "Start a journey",
            "Choose an option"
          )

        }

        "an unrecognised option is submitted" in {
          checkFormErrorIsDisplayed(
            performAction("journey" -> "1000"),
            "Start a journey",
            "Choose an option"
          )
        }

      }

      "throw an error" when {

        "there is an error saving a tax check" in {
          val loginData = loginDataNoExistingTaxChecks.copy(existingTaxChecks = List(existingTaxCheck))

          inSequence {
            mockTransformJourneyToLoginData(IndividualNoSA, expectedRedirectUrl)(loginData)
            mockSaveTaxCheck(existingTaxCheck)(Left(Error("")))
          }

          val result = performAction("journey" -> "0")
          a[RuntimeException] shouldBe thrownBy(await(result))
        }

        "there is an error logging in" in {
          inSequence {
            mockTransformJourneyToLoginData(IndividualNoSA, expectedRedirectUrl)(loginDataNoExistingTaxChecks)
            mockLogin(loginDataNoExistingTaxChecks)(Left(Error("")))
          }

          val result = performAction("journey" -> "0")
          a[RuntimeException] shouldBe thrownBy(await(result))
        }

      }

      "redirect to the correct url" when {

        "logging in is successful" in {
          JourneyStarterController.journeyOptions.zipWithIndex.foreach { case (journey, index) =>
            withClue(s"For journey $journey with index $index: ") {
              inSequence {
                mockTransformJourneyToLoginData(journey, expectedRedirectUrl)(loginDataNoExistingTaxChecks)
                mockLogin(loginDataNoExistingTaxChecks)(Right(session))
              }

              val result = performAction("journey" -> index.toString)
              checkIsRedirect(result, loginDataNoExistingTaxChecks.redirectUrl)
              await(result).session(FakeRequest()) shouldBe session
            }

          }

        }

        "saving a tax check is successful when required and logging in is successful" in {
          val loginData = loginDataNoExistingTaxChecks.copy(existingTaxChecks = List(existingTaxCheck))

          inSequence {
            mockTransformJourneyToLoginData(IndividualNoSA, expectedRedirectUrl)(loginData)
            mockSaveTaxCheck(existingTaxCheck)(Right(()))
            mockLogin(loginData)(Right(session))
          }

          val result = performAction("journey" -> "0")
          checkIsRedirect(result, loginData.redirectUrl)
          await(result).session(FakeRequest()) shouldBe session
        }

      }

    }

  }

}
