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

package uk.gov.hmrc.hecapplicantfrontend.services

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.controllers.{SessionSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, Error, HECSession, LicenceType, Name, UserAnswers}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyServiceSpec extends AnyWordSpec with Matchers with MockFactory with SessionSupport {

  val journeyService = new JourneyServiceImpl(mockSessionStore)

  def requestWithSessionData(s: HECSession): RequestWithSessionData[_] =
    RequestWithSessionData(AuthenticatedRequest(FakeRequest()), s)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val individualRetrievedData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None)

  "JourneyServiceImpl" when {

    "handling calls to 'firstPage'" must {

      "return the correct call" when afterWord("the user is") {

        "an individual" in {
          val session = HECSession(individualRetrievedData, UserAnswers.empty)
          journeyService.firstPage(session) shouldBe routes.ConfirmIndividualDetailsController
            .confirmIndividualDetails()
        }

        "a company" in {
          val session = HECSession(companyRetrievedData, UserAnswers.empty)
          journeyService.firstPage(session) shouldBe routes.LicenceDetailsController.licenceType()
        }

      }

    }

    "handling calls to 'updateAndNext'" must {

      "return an error" when {

        "the next page cannot be determined" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit(),
            session
          )

          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is an error updating the session" in {
          val currentSession                              = HECSession(individualRetrievedData, UserAnswers.empty)
          val updatedSession                              = HECSession(individualRetrievedData.copy(sautr = Some(SAUTR(""))), UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(currentSession)

          mockStoreSession(updatedSession)(Left(Error(new Exception("Oh no!"))))

          val result = journeyService.updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
            updatedSession
          )
          await(result.value) shouldBe a[Left[_, _]]
        }

      }

      "return the correct next page" when afterWord("the current page is") {

        "the tax check code page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
            session
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceType())
        }

        "the licence type page" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
            )

          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          mockStoreSession(updatedSession)(Right(()))

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.licenceType(),
            updatedSession
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.expiryDate())
        }

      }

    }

    "handling calls to 'previous'" must {

      "return an error" when {

        "no previous location can be found" in {
          val session                                     = HECSession(companyRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)
          assertThrows[RuntimeException](
            journeyService.previous(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit()
            )
          )
        }

      }

      "return the correct previous page" when afterWord("the current page is") {

        "the start endpoint" in {
          val session                                     = HECSession(companyRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.StartController.start()
          )

          result shouldBe routes.StartController.start()
        }

        "the confirm individual details page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
          )

          result shouldBe routes.StartController.start()
        }

        "the confirm individual details exit page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit()
          )

          result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
        }

        "the licence type page" when afterWord("the user is") {

          "an individual" in {
            val session                                     = HECSession(individualRetrievedData, UserAnswers.empty)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.LicenceDetailsController.licenceType()
            )

            result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
          }

          "a company" in {
            val session                                     = HECSession(companyRetrievedData, UserAnswers.empty)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.LicenceDetailsController.licenceType()
            )

            result shouldBe routes.StartController.start()
          }

        }

        "the licence type exit page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceTypeExit()
          )

          result shouldBe routes.LicenceDetailsController.licenceType()
        }

        "the licence expiry date page" in {
          val session                                     = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalDealerSite))
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.expiryDate()
          )

          result shouldBe routes.LicenceDetailsController.licenceType()
        }

      }

    }

  }

}
