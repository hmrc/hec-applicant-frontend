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
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.controllers.{SessionSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
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

        "the expiry page and the expiry date is within the past one year" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today().minusDays(10L)))
              )
            )

          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          mockStoreSession(updatedSession)(Right(()))

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.expiryDate(),
            updatedSession
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceTimeTrading())
        }

        "the expiry page and the expiry date is exactly one year ago" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today().minusYears(1L)))
              )
            )

          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          mockStoreSession(updatedSession)(Right(()))

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.expiryDate(),
            updatedSession
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceTimeTrading())
        }

        "the expiry page and the expiry date is beyond the past one year" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today().minusYears(1L).minusDays(1L)))
              )
            )

          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          mockStoreSession(updatedSession)(Right(()))

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.expiryDate(),
            updatedSession
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.expiryDateExit())
        }

        "the licence time trading page" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears))
            )

          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          mockStoreSession(updatedSession)(Right(()))

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.licenceTimeTrading(),
            updatedSession
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.recentLicenceLength())
        }

        "the licence validity period page and" when {

          "the licence type in the session is 'driver of taxis'" in {
            val answers        = UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
            val session        = HECSession(individualRetrievedData, answers)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToTwoYears))
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.recentLicenceLength(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.TaxSituationController.taxSituation())
          }

          "the licence type in the session is not 'driver of taxis'" in {
            List(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceType.ScrapMetalDealerSite,
              LicenceType.ScrapMetalMobileCollector
            ).foreach { licenceType =>
              withClue(s"For licence type $licenceType: ") {
                val answers        = UserAnswers.empty.copy(licenceType = Some(licenceType))
                val session        = HECSession(individualRetrievedData, answers)
                val updatedSession =
                  HECSession(
                    individualRetrievedData,
                    answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear))
                  )

                implicit val request: RequestWithSessionData[_] =
                  requestWithSessionData(session)

                mockStoreSession(updatedSession)(Right(()))

                val result = journeyService.updateAndNext(
                  routes.LicenceDetailsController.recentLicenceLength(),
                  updatedSession
                )
                await(result.value) shouldBe Right(routes.EntityTypeController.entityType())
              }
            }
          }

        }

        "the entity type page and" when {

          def test(
            retrievedData: RetrievedApplicantData,
            selectedEntityType: EntityType,
            expectedNext: Call
          ): Unit = {
            val answers        = UserAnswers.empty
            val session        = HECSession(retrievedData, answers)
            val updatedSession =
              HECSession(
                retrievedData,
                answers.copy(entityType = Some(selectedEntityType))
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.EntityTypeController.entityType(),
              updatedSession
            )
            await(result.value) shouldBe Right(expectedNext)
          }

          "the user is a individual but has selected company" in {
            test(individualRetrievedData, EntityType.Company, routes.EntityTypeController.wrongGGAccount())
          }

          "the user is a company but has selected individual" in {
            test(companyRetrievedData, EntityType.Individual, routes.EntityTypeController.wrongGGAccount())
          }

          "the user is a individual but has selected individual" in {
            test(individualRetrievedData, EntityType.Individual, routes.TaxSituationController.taxSituation())
          }

          "the user is a company but has selected company" in {
            test(companyRetrievedData, EntityType.Company, routes.TaxSituationController.taxSituation())
          }

        }

        "the tax situation page and" when {

          "the licence type in the session is 'driver of taxis'" in {
            val answers        = UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
            val session        = HECSession(individualRetrievedData, answers)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                answers.copy(taxSituation = Some(TaxSituation.PAYE))
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.TaxSituationController.taxSituation(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
          }

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

        "the licence expiry date exit page" in {
          val session                                     = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalDealerSite))
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.expiryDateExit()
          )

          result shouldBe routes.LicenceDetailsController.expiryDate()
        }

        "the licence time trading page when the session contains an licence expiry date which is not more than 1 year ago" in {
          val session                                     = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.ScrapMetalDealerSite),
              licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today()))
            )
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceTimeTrading()
          )

          result shouldBe routes.LicenceDetailsController.expiryDate()
        }

        "the entity type page" when {

          "the licence type selected is not 'driver of taxis'" in {
            List(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceType.ScrapMetalDealerSite,
              LicenceType.ScrapMetalMobileCollector
            ).foreach { licenceType =>
              withClue(s"For licence type $licenceType: ") {
                val session                                     = HECSession(
                  individualRetrievedData,
                  UserAnswers.empty.copy(
                    licenceType = Some(licenceType),
                    licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today()))
                  )
                )
                implicit val request: RequestWithSessionData[_] =
                  requestWithSessionData(session)

                val result = journeyService.previous(
                  routes.EntityTypeController.entityType()
                )

                result shouldBe routes.LicenceDetailsController.recentLicenceLength()
              }
            }
          }

        }

        "the wrong GG account page" in {
          val session                                     = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today())),
              entityType = Some(EntityType.Company)
            )
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.EntityTypeController.wrongGGAccount()
          )

          result shouldBe routes.EntityTypeController.entityType()
        }

        "the wrong entity type page" in {
          val session                                     = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today()))
            )
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.EntityTypeController.wrongEntityType()
          )

          result shouldBe routes.EntityTypeController.entityType()
        }

        "the tax situation page" when {

          "the licence type selected is 'driver of taxis'" in {

            val session                                     = HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today()))
              )
            )
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.TaxSituationController.taxSituation()
            )

            result shouldBe routes.LicenceDetailsController.recentLicenceLength()
          }

          "the licence type is not 'driver of taxis' and the entity type is correct" in {
            val session                                     = HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
                licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today())),
                entityType = Some(EntityType.Individual)
              )
            )
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.TaxSituationController.taxSituation()
            )

            result shouldBe routes.EntityTypeController.entityType()
          }

        }

      }

    }

  }

}
