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
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceExpiryDate, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
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
    IndividualRetrievedData(
      GGCredId(""),
      NINO(""),
      None,
      Name("", ""),
      DateOfBirth(LocalDate.now()),
      None,
      None
    )

  val companyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None)

  "JourneyServiceImpl" when {

    "handling calls to 'firstPage'" must {

      "return the correct call" when afterWord("the user is") {

        "an individual" in {
          val session = HECSession(individualRetrievedData, UserAnswers.empty, None)
          journeyService.firstPage(session) shouldBe routes.ConfirmIndividualDetailsController
            .confirmIndividualDetails()
        }

        "a company" in {
          val session = HECSession(companyRetrievedData, UserAnswers.empty, None)
          journeyService.firstPage(session) shouldBe routes.LicenceDetailsController.licenceType()
        }

      }

    }

    "handling calls to 'updateAndNext'" must {

      "return an error" when {

        "the next page cannot be determined" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit(),
            session
          )

          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is an error updating the session" in {
          val currentSession                              = HECSession(individualRetrievedData, UserAnswers.empty, None)
          val updatedSession                              =
            HECSession(individualRetrievedData.copy(sautr = Some(SAUTR(""))), UserAnswers.empty, None)
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

      "try to return the correct next page" when afterWord("the current page is") {

        "the tax check code page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
            session
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceType())
        }

        "the licence type page" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty, None)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)),
              None
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

        "the expiry page and" when {

          "no expiry date can be found in session" in {
            val answers                                     = UserAnswers.empty
            val session                                     = HECSession(individualRetrievedData, answers, None)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.updateAndNext(
                routes.LicenceDetailsController.expiryDate(),
                session
              )
            }
          }

          "the expiry date is within the past one year" in {
            val session        = HECSession(individualRetrievedData, UserAnswers.empty, None)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                UserAnswers.empty.copy(
                  licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                  licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today().minusDays(10L)))
                ),
                None
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
            val session        = HECSession(individualRetrievedData, UserAnswers.empty, None)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                UserAnswers.empty.copy(
                  licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                  licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today().minusYears(1L)))
                ),
                None
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
            val session        = HECSession(individualRetrievedData, UserAnswers.empty, None)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                UserAnswers.empty.copy(
                  licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
                  licenceExpiryDate = Some(LicenceExpiryDate(TimeUtils.today().minusYears(1L).minusDays(1L)))
                ),
                None
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
        }

        "the licence time trading page" in {
          val session        = HECSession(individualRetrievedData, UserAnswers.empty, None)
          val updatedSession =
            HECSession(
              individualRetrievedData,
              UserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears)),
              None
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

          "no licence type can be found in session" in {
            val answers        = UserAnswers.empty
            val session        = HECSession(individualRetrievedData, answers, None)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToTwoYears)),
                None
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.updateAndNext(
                routes.LicenceDetailsController.recentLicenceLength(),
                updatedSession
              )
            }
          }

          "the licence type in the session is 'driver of taxis'" in {
            val answers        = UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
            val session        = HECSession(individualRetrievedData, answers, None)
            val updatedSession =
              HECSession(
                individualRetrievedData,
                answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToTwoYears)),
                None
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
                val session        = HECSession(individualRetrievedData, answers, None)
                val updatedSession =
                  HECSession(
                    individualRetrievedData,
                    answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear)),
                    None
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
            val session        = HECSession(retrievedData, answers, None)
            val updatedSession =
              HECSession(
                retrievedData,
                answers.copy(entityType = Some(selectedEntityType)),
                None
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

          "no entity type can be found in session" in {
            val answers                                     = UserAnswers.empty
            val session                                     = HECSession(individualRetrievedData, answers, None)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.updateAndNext(
                routes.EntityTypeController.entityType(),
                session
              )
            }
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

          val individualWoStatus =
            IndividualRetrievedData(
              GGCredId(""),
              NINO(""),
              Some(SAUTR("utr")),
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None,
              None
            )

          def individualWithStatus(status: SAStatus = SAStatus.NoticeToFileIssued): IndividualRetrievedData =
            individualWoStatus.copy(
              saStatus = Some(SAStatusResponse(SAUTR(""), TaxYear(2020), status))
            )

          val answers = UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))

          def answersWithTaxSituation(taxSituation: TaxSituation) = UserAnswers.empty.copy(
            licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
            taxSituation = Some(taxSituation)
          )

          "tax situation is missing" in {
            val session        = HECSession(individualWithStatus(SAStatus.NoticeToFileIssued), answers, None)
            val updatedSession = HECSession(individualWithStatus(SAStatus.ReturnFound), answers, None)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException](
              journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation(),
                updatedSession
              )
            )
          }

          def testPAYENotChargeable(taxSituation: TaxSituation) = {
            val session        = HECSession(individualWithStatus(), answers, None)
            val updatedSession = HECSession(individualWithStatus(), answersWithTaxSituation(taxSituation), None)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.TaxSituationController.taxSituation(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
          }

          "tax situation is PAYE" in {
            testPAYENotChargeable(TaxSituation.PAYE)
          }

          "tax situation is Not Chargeable" in {
            testPAYENotChargeable(TaxSituation.NotChargeable)
          }

          def testsForSelfAssessment(taxSituation: TaxSituation) = {
            "SAUTR is present but there is no SA status response" in {
              val answersWithTaxSituation = answers.copy(taxSituation = Some(taxSituation))
              val session                 = HECSession(
                individualRetrievedData.copy(saStatus = None, sautr = Some(SAUTR(""))),
                answersWithTaxSituation,
                None
              )

              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(
                  routes.TaxSituationController.taxSituation(),
                  session
                )
              }
            }

            "SAUTR is missing" in {
              val individualWithoutSautr  = individualRetrievedData
              val answersWithTaxSituation = answers.copy(taxSituation = Some(taxSituation))
              val session                 = HECSession(individualWithoutSautr, answers, None)
              val updatedSession          = HECSession(individualWithoutSautr, answersWithTaxSituation, None)

              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              mockStoreSession(updatedSession)(Right(()))

              val result = journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation(),
                updatedSession
              )
              await(result.value) shouldBe Right(routes.SAController.sautrNotFoundExit())
            }

            "applicant type is company" in {
              val session        = HECSession(companyRetrievedData, answers, None)
              val updatedSession =
                HECSession(companyRetrievedData, answersWithTaxSituation(taxSituation), None)

              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              assertThrows[RuntimeException](
                journeyService.updateAndNext(
                  routes.TaxSituationController.taxSituation(),
                  updatedSession
                )
              )
            }

            "SA status = ReturnFound" in {
              val individualReturnFound = individualWithStatus(SAStatus.ReturnFound)
              val session               = HECSession(individualReturnFound, answers, None)
              val updatedSession        = HECSession(individualReturnFound, answersWithTaxSituation(taxSituation), None)

              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              mockStoreSession(updatedSession)(Right(()))
              val result = journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation(),
                updatedSession
              )
              await(result.value) shouldBe Right(routes.SAController.confirmYourIncome())
            }

            "SA status = NoReturnFound" in {
              val individualNoReturnFound = individualWithStatus(SAStatus.NoReturnFound)
              val session                 = HECSession(individualNoReturnFound, answers, None)
              val updatedSession          = HECSession(individualNoReturnFound, answersWithTaxSituation(taxSituation), None)

              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              mockStoreSession(updatedSession)(Right(()))
              val result = journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation(),
                updatedSession
              )
              await(result.value) shouldBe Right(routes.SAController.noReturnFoundExit())
            }

            "SA status = NoticeToFileIssued" in {
              val individualNoticeIssued = individualWithStatus(SAStatus.NoticeToFileIssued)
              val session                = HECSession(individualNoticeIssued, answers, None)
              val updatedSession         = HECSession(individualNoticeIssued, answersWithTaxSituation(taxSituation), None)

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

          "tax situation is SA" when {
            testsForSelfAssessment(TaxSituation.SA)
          }

          "tax situation is SAPAYE" when {
            testsForSelfAssessment(TaxSituation.SAPAYE)
          }
        }

        "the check your answers page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.CheckYourAnswersController.checkYourAnswers(),
            session
          )
          await(result.value) shouldBe Right(routes.TaxCheckCompleteController.taxCheckComplete())
        }

      }

      "convert incomplete answers to complete answers when all questions have been answered and" when {

        "the user has not selected a licence type which both individuals and companies can have" in {
          val completeAnswers = CompleteUserAnswers(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceExpiryDate(TimeUtils.today()),
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            TaxSituation.PAYE,
            None
          )

          val incompleteAnswers = IncompleteUserAnswers(
            Some(completeAnswers.licenceType),
            Some(completeAnswers.licenceExpiryDate),
            Some(completeAnswers.licenceTimeTrading),
            Some(completeAnswers.licenceValidityPeriod),
            Some(completeAnswers.taxSituation),
            None
          )

          val individualData = individualRetrievedData.copy(
            sautr = Some(SAUTR("utr")),
            saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoticeToFileIssued))
          )

          val session = HECSession(individualData, incompleteAnswers, None)

          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.licenceTimeTrading(),
            session
          )
          await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())

        }

        "the user has selected a licence type which both individuals and companies can have" in {
          List(
            LicenceType.ScrapMetalDealerSite,
            LicenceType.ScrapMetalMobileCollector,
            LicenceType.OperatorOfPrivateHireVehicles
          ).foreach { licenceType =>
            withClue(s"For licence type $licenceType: ") {
              val completeAnswers = CompleteUserAnswers(
                licenceType,
                LicenceExpiryDate(TimeUtils.today()),
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                TaxSituation.PAYE,
                Some(EntityType.Company)
              )

              val incompleteAnswers = IncompleteUserAnswers(
                Some(completeAnswers.licenceType),
                Some(completeAnswers.licenceExpiryDate),
                Some(completeAnswers.licenceTimeTrading),
                Some(completeAnswers.licenceValidityPeriod),
                Some(completeAnswers.taxSituation),
                Some(EntityType.Company)
              )

              val session                                     = HECSession(companyRetrievedData, incompleteAnswers, None)
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

              val result = journeyService.updateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session
              )
              await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())

            }

          }

        }

      }

      "not convert incomplete answers to complete answers when all questions have been answered and" when {

        "the licence expiry date is not valid" in {
          val incompleteAnswers = IncompleteUserAnswers(
            Some(LicenceType.DriverOfTaxisAndPrivateHires),
            Some(LicenceExpiryDate(TimeUtils.today().minusMonths(13L))),
            Some(LicenceTimeTrading.ZeroToTwoYears),
            Some(LicenceValidityPeriod.UpToOneYear),
            Some(TaxSituation.PAYE),
            None
          )

          val session                                     = HECSession(individualRetrievedData, incompleteAnswers, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.LicenceDetailsController.expiryDate(),
            session
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.expiryDateExit())
        }

        "the selected entity type is not consistent with the entity type retrieved from the GG creds" in {
          val incompleteAnswers = IncompleteUserAnswers(
            Some(LicenceType.OperatorOfPrivateHireVehicles),
            Some(LicenceExpiryDate(TimeUtils.today().minusMonths(13L))),
            Some(LicenceTimeTrading.ZeroToTwoYears),
            Some(LicenceValidityPeriod.UpToOneYear),
            Some(TaxSituation.PAYE),
            Some(EntityType.Company)
          )

          val session                                     = HECSession(individualRetrievedData, incompleteAnswers, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.EntityTypeController.entityType(),
            session
          )
          await(result.value) shouldBe Right(routes.EntityTypeController.wrongGGAccount())
        }

      }

    }

    "handling calls to 'previous'" must {

      "return an error" when {

        "no previous location can be found" in {
          val session                                     = HECSession(companyRetrievedData, UserAnswers.empty, None)
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
          val session                                     = HECSession(companyRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.StartController.start()
          )

          result shouldBe routes.StartController.start()
        }

        "the confirm individual details page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
          )

          result shouldBe routes.StartController.start()
        }

        "the confirm individual details exit page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit()
          )

          result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
        }

        "the licence type page" when afterWord("the user is") {

          "an individual" in {
            val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.LicenceDetailsController.licenceType()
            )

            result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
          }

          "a company" in {
            val session                                     = HECSession(companyRetrievedData, UserAnswers.empty, None)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.LicenceDetailsController.licenceType()
            )

            result shouldBe routes.StartController.start()
          }

        }

        "the licence type exit page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
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
            UserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalDealerSite)),
            None
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
            UserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalDealerSite)),
            None
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
            ),
            None
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
                  ),
                  None
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
            ),
            None
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
            ),
            None
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
              ),
              None
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
              ),
              None
            )
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.TaxSituationController.taxSituation()
            )

            result shouldBe routes.EntityTypeController.entityType()
          }

        }

        "the check your answers page" when {

          def testPrevPageIsTaxSituation(session: HECSession) = {
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.CheckYourAnswersController.checkYourAnswers()
            )

            result shouldBe routes.TaxSituationController.taxSituation()
          }

          val individualDataWithStatus = individualRetrievedData.copy(
            sautr = Some(SAUTR("utr")),
            saStatus = Some(SAStatusResponse(SAUTR(""), TaxYear(2020), SAStatus.NoticeToFileIssued))
          )

          def userAnswers(licenceType: LicenceType, taxSituation: TaxSituation, entityType: Option[EntityType]) =
            CompleteUserAnswers(
              licenceType,
              LicenceExpiryDate(TimeUtils.today()),
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              taxSituation,
              entityType
            )

          "tax situation = SA & SA status = NoticeToFileIssued" in {
            val session = HECSession(
              individualDataWithStatus,
              userAnswers(LicenceType.DriverOfTaxisAndPrivateHires, TaxSituation.SA, None),
              None
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = SAPAYE & SA status = NoticeToFileIssued" in {
            val session = HECSession(
              individualDataWithStatus,
              userAnswers(LicenceType.DriverOfTaxisAndPrivateHires, TaxSituation.SAPAYE, None),
              None
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = PAYE" in {
            val session = HECSession(
              individualDataWithStatus,
              userAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                TaxSituation.PAYE,
                None
              ),
              None
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = Not Chargeable" in {
            val session = HECSession(
              individualDataWithStatus,
              userAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                TaxSituation.NotChargeable,
                None
              ),
              None
            )

            testPrevPageIsTaxSituation(session)
          }
        }

        "confirm your SA income page" when {

          def test(taxSituation: TaxSituation) = {
            val session = buildIndividualSession(taxSituation, SAStatus.ReturnFound)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.SAController.confirmYourIncome()
            )

            result shouldBe routes.TaxSituationController.taxSituation()
          }

          def testThrows(taxSituation: TaxSituation) = {
            val session = buildIndividualSession(taxSituation, SAStatus.ReturnFound)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.previous(
                routes.SAController.confirmYourIncome()
              )
            }
          }

          "tax situation = SA & SA status = ReturnFound" in {
            test(TaxSituation.SA)
          }

          "tax situation = SAPAYE & SA status = ReturnFound" in {
            test(TaxSituation.SAPAYE)
          }

          "tax situation = PAYE" in {
            testThrows(TaxSituation.PAYE)
          }

          "tax situation = Not Chargeable" in {
            testThrows(TaxSituation.NotChargeable)
          }
        }

        "no return found page" when {

          def test(taxSituation: TaxSituation) = {
            val session = buildIndividualSession(taxSituation, SAStatus.NoReturnFound)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.SAController.noReturnFoundExit()
            )

            result shouldBe routes.TaxSituationController.taxSituation()
          }

          "tax situation = SA & SA status = NoReturnFound" in {
            test(TaxSituation.SA)
          }

          "tax situation = SAPAYE & SA status = NoReturnFound" in {
            test(TaxSituation.SAPAYE)
          }

          def testThrows(taxSituation: TaxSituation) = {
            val session = buildIndividualSession(taxSituation, SAStatus.NoReturnFound)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.previous(
                routes.SAController.noReturnFoundExit()
              )
            }
          }

          "tax situation = PAYE" in {
            testThrows(TaxSituation.PAYE)
          }

          "tax situation = Not Chargeable" in {
            testThrows(TaxSituation.NotChargeable)
          }
        }

        def buildIndividualSession(taxSituation: TaxSituation, saStatus: SAStatus): HECSession = {
          val individualData = IndividualRetrievedData(
            GGCredId(""),
            NINO(""),
            Some(SAUTR("utr")),
            Name("", ""),
            DateOfBirth(LocalDate.now()),
            None,
            Some(SAStatusResponse(SAUTR(""), TaxYear(2020), saStatus))
          )
          HECSession(
            individualData,
            IncompleteUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceExpiryDate(TimeUtils.today())),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToOneYear),
              Some(taxSituation),
              None
            ),
            None
          )
        }

      }

      "return the check your answers page" when {

        "the answers in the session are complete and the current page is not the check your answers page" in {
          val completeAnswers                             = CompleteUserAnswers(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceExpiryDate(TimeUtils.today()),
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            TaxSituation.PAYE,
            Some(EntityType.Individual)
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(
              HECSession(individualRetrievedData, completeAnswers, None)
            )

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceType()
          )

          result shouldBe routes.CheckYourAnswersController.checkYourAnswers()
        }

      }

    }

  }

}
