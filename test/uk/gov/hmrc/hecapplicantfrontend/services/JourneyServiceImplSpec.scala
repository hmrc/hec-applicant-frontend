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

import java.time.LocalDate
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.controllers.{SessionSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.Company
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

class JourneyServiceSpec extends AnyWordSpec with Matchers with MockFactory with SessionSupport {

  val journeyService: JourneyServiceImpl = new JourneyServiceImpl(mockSessionStore)

  def requestWithSessionData(s: HECSession): RequestWithSessionData[_] =
    RequestWithSessionData(AuthenticatedRequest(FakeRequest()), s)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val individualRetrievedData: IndividualRetrievedData =
    IndividualRetrievedData(
      GGCredId(""),
      NINO(""),
      None,
      Name("", ""),
      DateOfBirth(LocalDate.now()),
      None,
      None
    )

  val companyRetrievedData: CompanyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None, None)

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

        "the licence type page" when afterWord("the user is") {

          "an Individual" in {

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
            await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceTimeTrading())
          }

          "a Company" in {

            val session        = HECSession(companyRetrievedData, UserAnswers.empty, None)
            val updatedSession =
              session.copy(
                userAnswers = UserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.licenceType(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceTimeTrading())
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

          "the user is a individual and  has selected individual" in {
            test(individualRetrievedData, EntityType.Individual, routes.TaxSituationController.taxSituation())
          }

          "the user is a company and  has selected company" in {
            test(companyRetrievedData, EntityType.Company, routes.CRNController.companyRegistrationNumber())
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

          def testsForSelfAssessment(taxSituation: TaxSituation): Unit = {
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
              await(result.value) shouldBe Right(routes.SAController.sautrNotFound())
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
              await(result.value) shouldBe Right(routes.SAController.saIncomeStatement())
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
              await(result.value) shouldBe Right(routes.SAController.noReturnFound())
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

        "the SA income statement page" in {
          val session                                     = HECSession(individualRetrievedData, UserAnswers.empty, None)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.SAController.saIncomeStatement(),
            session
          )
          await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
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

        "the company registration number  page" when {

          def testCrnNextpage(companyName: Option[CompanyHouseName], resultCall: Call) = {
            val session        = HECSession(companyRetrievedData, UserAnswers.empty, None)
            val updatedSession =
              HECSession(
                companyRetrievedData.copy(companyName = companyName),
                UserAnswers.empty.copy(crn = Some(CRN("1234567"))),
                None
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CRNController.companyRegistrationNumber(),
              updatedSession
            )
            await(result.value) shouldBe Right(resultCall)
          }

          "the company is found" in {
            testCrnNextpage(
              Some(CompanyHouseName("Test tech Ltd")),
              routes.CompanyDetailsController.confirmCompanyDetails()
            )
          }

          "the company is  not found" in {
            testCrnNextpage(None, routes.CompanyDetailsNotFoundController.companyNotFound())
          }

        }

      }

      "convert incomplete answers to complete answers when all questions have been answered and" when {

        "the user has selected an individual only licence type" in {
          val completeAnswers = CompleteUserAnswers(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            Some(TaxSituation.PAYE),
            None,
            None,
            None
          )

          val incompleteAnswers = IncompleteUserAnswers(
            Some(completeAnswers.licenceType),
            Some(completeAnswers.licenceTimeTrading),
            Some(completeAnswers.licenceValidityPeriod),
            completeAnswers.taxSituation,
            completeAnswers.saIncomeDeclared,
            None,
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
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(TaxSituation.PAYE),
                Some(IncomeDeclared.Yes),
                Some(EntityType.Company),
                None
              )

              val incompleteAnswers = IncompleteUserAnswers(
                Some(completeAnswers.licenceType),
                Some(completeAnswers.licenceTimeTrading),
                Some(completeAnswers.licenceValidityPeriod),
                completeAnswers.taxSituation,
                Some(IncomeDeclared.Yes),
                Some(EntityType.Company),
                None
              )

              val session                                     = HECSession(individualRetrievedData, incompleteAnswers, None)
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

        "the user has selected a non-SA tax situation" in {
          List(
            TaxSituation.PAYE,
            TaxSituation.NotChargeable
          ).foreach { taxSituation =>
            withClue(s"For tax situation $taxSituation: ") {
              val completeAnswers = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(taxSituation),
                None,
                None,
                None
              )

              val incompleteAnswers = IncompleteUserAnswers(
                Some(completeAnswers.licenceType),
                Some(completeAnswers.licenceTimeTrading),
                Some(completeAnswers.licenceValidityPeriod),
                completeAnswers.taxSituation,
                completeAnswers.saIncomeDeclared,
                completeAnswers.entityType,
                None
              )

              val session = HECSession(individualRetrievedData, incompleteAnswers, None)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
              mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

              val result = journeyService.updateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session
              )
              await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
            }
          }
        }

        "the user has selected an SA tax situation, SA status is ReturnFound & SA income declared is present" in {
          List(
            TaxSituation.SA,
            TaxSituation.SAPAYE
          ).foreach { taxSituation =>
            withClue(s"For tax situation $taxSituation: ") {
              val completeAnswers = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(taxSituation),
                Some(IncomeDeclared.Yes),
                None,
                None
              )

              val incompleteAnswers = IncompleteUserAnswers(
                Some(completeAnswers.licenceType),
                Some(completeAnswers.licenceTimeTrading),
                Some(completeAnswers.licenceValidityPeriod),
                completeAnswers.taxSituation,
                completeAnswers.saIncomeDeclared,
                completeAnswers.entityType,
                None
              )

              val individualData = individualRetrievedData.copy(saStatus =
                Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )
              val session        = HECSession(individualData, incompleteAnswers, None)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
              mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

              val result = journeyService.updateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session
              )
              await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
            }
          }
        }

        "the user has selected an SA tax situation, SA status != ReturnFound" in {
          List(
            TaxSituation.SA,
            TaxSituation.SAPAYE
          ).foreach { taxSituation =>
            withClue(s"For tax situation $taxSituation: ") {
              val completeAnswers = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(taxSituation),
                None,
                None,
                None
              )

              val incompleteAnswers = IncompleteUserAnswers(
                Some(completeAnswers.licenceType),
                Some(completeAnswers.licenceTimeTrading),
                Some(completeAnswers.licenceValidityPeriod),
                completeAnswers.taxSituation,
                completeAnswers.saIncomeDeclared,
                completeAnswers.entityType,
                None
              )

              val individualData = individualRetrievedData.copy(saStatus =
                Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoReturnFound))
              )
              val session        = HECSession(individualData, incompleteAnswers, None)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
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

        "the selected entity type is not consistent with the entity type retrieved from the GG creds" in {
          val incompleteAnswers = IncompleteUserAnswers(
            Some(LicenceType.OperatorOfPrivateHireVehicles),
            Some(LicenceTimeTrading.ZeroToTwoYears),
            Some(LicenceValidityPeriod.UpToOneYear),
            Some(TaxSituation.PAYE),
            Some(IncomeDeclared.Yes),
            Some(EntityType.Company),
            None
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

        "the licence time trading page when the session contains an licence expiry date which is not more than 1 year ago" in {
          val session                                     = HECSession(
            individualRetrievedData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.ScrapMetalDealerSite)
            ),
            None
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceTimeTrading()
          )

          result shouldBe routes.LicenceDetailsController.licenceType()
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
                    licenceType = Some(licenceType)
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
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
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
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
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
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              Some(taxSituation),
              Some(IncomeDeclared.Yes),
              entityType,
              None
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
              routes.SAController.saIncomeStatement()
            )

            result shouldBe routes.TaxSituationController.taxSituation()
          }

          def testThrows(taxSituation: TaxSituation) = {
            val session = buildIndividualSession(taxSituation, SAStatus.ReturnFound)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.previous(
                routes.SAController.saIncomeStatement()
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
              routes.SAController.noReturnFound()
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
                routes.SAController.noReturnFound()
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

        "the company registration number page" in {
          val session                                     = HECSession(
            companyRetrievedData.copy(companyName = Some(CompanyHouseName("Test Tech Ltd"))),
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            ),
            None
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CRNController.companyRegistrationNumber()
          )

          result shouldBe routes.EntityTypeController.entityType
        }

        "the confirm company details page" in {
          val session                                     = HECSession(
            companyRetrievedData.copy(companyName = Some(CompanyHouseName("Test Tech Ltd"))),
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            ),
            None
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CompanyDetailsController.confirmCompanyDetails()
          )

          result shouldBe routes.CRNController.companyRegistrationNumber()

        }

        "the company details not found page" in {
          val session                                     = HECSession(
            companyRetrievedData,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            ),
            None
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CompanyDetailsNotFoundController.companyNotFound()
          )

          result shouldBe routes.CRNController.companyRegistrationNumber()

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
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToOneYear),
              Some(taxSituation),
              Some(IncomeDeclared.Yes),
              None,
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
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            Some(TaxSituation.PAYE),
            Some(IncomeDeclared.Yes),
            Some(EntityType.Individual),
            None
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

    "JourneyServiceImpl.allAnswersComplete" must {
      val incompleteAnswersBase = IncompleteUserAnswers(
        licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
        licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears),
        licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
        taxSituation = Some(TaxSituation.PAYE),
        saIncomeDeclared = None,
        entityType = None,
        None
      )

      "return true for individual licence type & entity type is missing" in {
        JourneyServiceImpl.allAnswersComplete(
          incompleteUserAnswers = incompleteAnswersBase,
          retrievedUserData = individualRetrievedData
        ) shouldBe true
      }

      "return false for individual licence type & entity type is present" in {
        JourneyServiceImpl.allAnswersComplete(
          incompleteUserAnswers = incompleteAnswersBase.copy(entityType = Some(EntityType.Individual)),
          retrievedUserData = individualRetrievedData
        ) shouldBe false
      }

      "return false for company licence type & entity type is missing" in {
        List(
          LicenceType.OperatorOfPrivateHireVehicles,
          LicenceType.ScrapMetalDealerSite,
          LicenceType.ScrapMetalMobileCollector
        ) foreach { licenceType =>
          withClue(s"for $licenceType") {
            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(licenceType = Some(licenceType)),
              retrievedUserData = companyRetrievedData
            ) shouldBe false
          }
        }
      }

      "return true for company licence type & entity type is present" in {
        List(
          LicenceType.OperatorOfPrivateHireVehicles,
          LicenceType.ScrapMetalDealerSite,
          LicenceType.ScrapMetalMobileCollector
        ) foreach { licenceType =>
          withClue(s"for $licenceType") {
            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                licenceType = Some(licenceType),
                entityType = Some(EntityType.Company),
                taxSituation = None,
                saIncomeDeclared = None,
                crn = Some(CRN("1234567"))
              ),
              retrievedUserData = companyRetrievedData
            ) shouldBe true
          }
        }
      }

      "return true for tax situation = non-SA (irrespective of whether income declared is missing or present)" in {
        List(
          TaxSituation.PAYE,
          TaxSituation.NotChargeable
        ) foreach { taxSituation =>
          withClue(s"for $taxSituation") {
            // income declared is missing
            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                taxSituation = Some(taxSituation),
                saIncomeDeclared = None
              ),
              retrievedUserData = individualRetrievedData
            ) shouldBe true

            // income declared is present
            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                taxSituation = Some(taxSituation),
                saIncomeDeclared = Some(IncomeDeclared.Yes)
              ),
              retrievedUserData = individualRetrievedData
            ) shouldBe true
          }
        }
      }

      "return true for tax situation = SA & status != ReturnFound (irrespective of whether income declared is missing or present)" in {
        val saTaxSituations = List(
          TaxSituation.SA,
          TaxSituation.SAPAYE
        )
        val saStatuses      = List[SAStatus](
          SAStatus.NoticeToFileIssued,
          SAStatus.NoReturnFound
        )

        val permutations = for {
          ts     <- saTaxSituations
          status <- saStatuses
        } yield ts -> status

        permutations foreach { case (taxSituation, saStatus) =>
          withClue(s"for $taxSituation & $saStatus") {
            val individualData =
              individualRetrievedData.copy(saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), saStatus)))

            // income declared is missing
            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                taxSituation = Some(taxSituation),
                saIncomeDeclared = None
              ),
              retrievedUserData = individualData
            ) shouldBe true

            // income declared is present
            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                taxSituation = Some(taxSituation),
                saIncomeDeclared = Some(IncomeDeclared.Yes)
              ),
              retrievedUserData = individualData
            ) shouldBe true
          }
        }
      }

      "return true for tax situation = SA & status = ReturnFound & income declared is present)" in {
        List(
          TaxSituation.SA,
          TaxSituation.SAPAYE
        ) foreach { taxSituation =>
          withClue(s"for $taxSituation") {
            val individualData = individualRetrievedData.copy(saStatus =
              Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
            )

            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                taxSituation = Some(taxSituation),
                saIncomeDeclared = Some(IncomeDeclared.Yes)
              ),
              retrievedUserData = individualData
            ) shouldBe true
          }
        }
      }

      "return false for tax situation = SA & status = ReturnFound & income declared is missing)" in {
        List(
          TaxSituation.SA,
          TaxSituation.SAPAYE
        ) foreach { taxSituation =>
          withClue(s"for $taxSituation") {
            val individualData = individualRetrievedData.copy(saStatus =
              Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
            )

            JourneyServiceImpl.allAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase.copy(
                taxSituation = Some(taxSituation),
                saIncomeDeclared = None
              ),
              retrievedUserData = individualData
            ) shouldBe false
          }
        }
      }
    }

  }

}
