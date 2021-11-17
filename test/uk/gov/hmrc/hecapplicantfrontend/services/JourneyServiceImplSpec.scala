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

import play.api.mvc.Call
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.controllers.{ControllerSpec, SessionSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.{Company, Individual}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation.PAYE
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod.UpToOneYear
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyServiceSpec extends ControllerSpec with SessionSupport {

  val journeyService: JourneyServiceImpl = new JourneyServiceImpl(mockSessionStore)

  val taxCheckStartDateTime = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))

  def requestWithSessionData(s: HECSession): RequestWithSessionData[_] =
    RequestWithSessionData(AuthenticatedRequest(FakeRequest()), s)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val individualLoginData: IndividualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData: CompanyLoginData =
    CompanyLoginData(GGCredId(""), None, None)

  val companyLoginData1: CompanyLoginData =
    CompanyLoginData(GGCredId(""), Some(CTUTR("4444444444")), None)

  "JourneyServiceImpl" when {

    "handling calls to 'firstPage'" when {

      "tax checks list is not empty" must {

        val taxChecks = List(
          TaxCheckListItem(
            LicenceType.DriverOfTaxisAndPrivateHires,
            HECTaxCheckCode("some-code"),
            LocalDate.now(),
            ZonedDateTime.now()
          )
        )

        "return the correct call" when afterWord("the user is") {

          "an individual" in {
            val session = IndividualHECSession.newSession(individualLoginData).copy(unexpiredTaxChecks = taxChecks)
            journeyService.firstPage(session) shouldBe routes.ConfirmIndividualDetailsController
              .confirmIndividualDetails()
          }

          "a company" in {
            val session = CompanyHECSession.newSession(companyLoginData).copy(unexpiredTaxChecks = taxChecks)
            journeyService.firstPage(session) shouldBe routes.TaxChecksListController.unexpiredTaxChecks()
          }
        }
      }

      "tax checks list is empty" must {

        "return the correct call" when afterWord("the user is") {

          "an individual" in {
            val session = IndividualHECSession.newSession(individualLoginData)
            journeyService.firstPage(session) shouldBe routes.ConfirmIndividualDetailsController
              .confirmIndividualDetails()
          }

          "a company" in {
            val session = CompanyHECSession.newSession(companyLoginData)
            journeyService.firstPage(session) shouldBe routes.LicenceDetailsController.licenceType()
          }

        }
      }

    }

    "handling calls to 'updateAndNext'" must {

      "return an error" when {

        "the next page cannot be determined" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit(),
            session
          )

          await(result.value) shouldBe a[Left[_, _]]
        }

        "there is an error updating the session" in {
          val currentSession                              = IndividualHECSession.newSession(individualLoginData)
          val updatedSession                              = IndividualHECSession.newSession(individualLoginData.copy(sautr = Some(SAUTR(""))))
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

        "the confirm your details page" when {

          "there are no preexisting tax check codes" in {
            val session                                     = IndividualHECSession.newSession(individualLoginData)
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
              session
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceType())
          }

          "there are preexisting tax check codes" in {
            val taxChecks                                   = List(
              TaxCheckListItem(
                LicenceType.DriverOfTaxisAndPrivateHires,
                HECTaxCheckCode("some-code"),
                LocalDate.now(),
                ZonedDateTime.now()
              )
            )
            val session                                     = IndividualHECSession.newSession(individualLoginData).copy(unexpiredTaxChecks = taxChecks)
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails(),
              session
            )
            await(result.value) shouldBe Right(routes.TaxChecksListController.unexpiredTaxChecks())
          }
        }

        "the tax check codes page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.TaxChecksListController.unexpiredTaxChecks(),
            session
          )
          await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceType())
        }

        "the licence type page" when afterWord("the user is") {

          "an Individual" in {

            val session        = IndividualHECSession.newSession(individualLoginData)
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
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

            val session        = CompanyHECSession.newSession(companyLoginData)
            val updatedSession =
              session.copy(
                userAnswers =
                  CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
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

        "the licence time trading page" when {

          "when all user answers are not complete" in {
            val session        = IndividualHECSession.newSession(individualLoginData)
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears))
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

          "when all user answers are complete" in {
            val session        = IndividualHECSession.newSession(individualLoginData)
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                Fixtures.completeIndividualUserAnswers(
                  licenceType = DriverOfTaxisAndPrivateHires,
                  licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                  licenceValidityPeriod = UpToOneYear,
                  taxSituation = PAYE,
                  saIncomeDeclared = Some(YesNoAnswer.Yes),
                  entityType = Some(Individual)
                ),
                None,
                Some(taxCheckStartDateTime)
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.licenceTimeTrading(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
          }

        }

        "the licence validity period page and" when {

          "no licence type can be found in session" in {
            val answers        = IndividualUserAnswers.empty
            val session        =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToTwoYears))
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
            val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
            val session        =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
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
                val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(licenceType))
                val session        =
                  Fixtures.individualHECSession(
                    individualLoginData,
                    IndividualRetrievedJourneyData.empty,
                    answers
                  )
                val updatedSession =
                  Fixtures.individualHECSession(
                    individualLoginData,
                    IndividualRetrievedJourneyData.empty,
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
            loginData: LoginData,
            selectedEntityType: EntityType,
            expectedNext: Call
          ): Unit = {
            val session = loginData match {
              case i: IndividualLoginData => IndividualHECSession.newSession(i)
              case c: CompanyLoginData    => CompanyHECSession.newSession(c)
            }

            val updatedSession =
              session.fold(
                _.copy(userAnswers = IndividualUserAnswers.empty.copy(entityType = Some(selectedEntityType))),
                _.copy(userAnswers = CompanyUserAnswers.empty.copy(entityType = Some(selectedEntityType)))
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
            val answers                                     = IndividualUserAnswers.empty
            val session                                     =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
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
            test(individualLoginData, EntityType.Company, routes.EntityTypeController.wrongGGAccount())
          }

          "the user is a company but has selected individual" in {
            test(companyLoginData, EntityType.Individual, routes.EntityTypeController.wrongGGAccount())
          }

          "the user is a individual and  has selected individual" in {
            test(individualLoginData, EntityType.Individual, routes.TaxSituationController.taxSituation())
          }

          "the user is a company and  has selected company" in {
            test(companyLoginData, EntityType.Company, routes.CRNController.companyRegistrationNumber())
          }

        }

        "the tax situation page and" when {

          val individualWithSautr =
            IndividualLoginData(
              GGCredId(""),
              NINO(""),
              Some(SAUTR("utr")),
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None
            )

          def retrievedJourneyDataWithSaStatus(
            status: SAStatus = SAStatus.NoticeToFileIssued
          ): IndividualRetrievedJourneyData =
            IndividualRetrievedJourneyData.empty.copy(
              saStatus = Some(SAStatusResponse(SAUTR(""), TaxYear(2020), status))
            )

          val individualAnswers =
            IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
          val companyAnswers    =
            CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))

          def answersWithTaxSituation(taxSituation: TaxSituation) = IndividualUserAnswers.empty.copy(
            licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
            taxSituation = Some(taxSituation)
          )

          "tax situation is missing" in {
            val session        = Fixtures.individualHECSession(
              individualWithSautr,
              retrievedJourneyDataWithSaStatus(SAStatus.NoticeToFileIssued),
              individualAnswers
            )
            val updatedSession =
              Fixtures.individualHECSession(
                individualWithSautr,
                retrievedJourneyDataWithSaStatus(SAStatus.ReturnFound),
                individualAnswers
              )

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
            val session        =
              Fixtures.individualHECSession(
                individualWithSautr,
                retrievedJourneyDataWithSaStatus(),
                individualAnswers
              )
            val updatedSession = Fixtures.individualHECSession(
              individualWithSautr,
              retrievedJourneyDataWithSaStatus(),
              answersWithTaxSituation(taxSituation)
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

          "tax situation is PAYE" in {
            testPAYENotChargeable(TaxSituation.PAYE)
          }

          "tax situation is Not Chargeable" in {
            testPAYENotChargeable(TaxSituation.NotChargeable)
          }

          def testsForSelfAssessment(taxSituation: TaxSituation): Unit = {
            "SAUTR is present but there is no SA status response" in {
              val answersWithTaxSituation = individualAnswers.copy(taxSituation = Some(taxSituation))
              val session                 = Fixtures.individualHECSession(
                individualLoginData.copy(sautr = Some(SAUTR(""))),
                IndividualRetrievedJourneyData.empty,
                answersWithTaxSituation
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
              val individualWithoutSautr  = individualLoginData
              val answersWithTaxSituation = individualAnswers.copy(taxSituation = Some(taxSituation))
              val session                 =
                Fixtures.individualHECSession(
                  individualWithoutSautr,
                  IndividualRetrievedJourneyData.empty,
                  individualAnswers
                )
              val updatedSession          =
                Fixtures.individualHECSession(
                  individualWithoutSautr,
                  IndividualRetrievedJourneyData.empty,
                  answersWithTaxSituation
                )

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
              val session        =
                Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, companyAnswers)
              val updatedSession =
                Fixtures.companyHECSession(
                  companyLoginData,
                  CompanyRetrievedJourneyData.empty,
                  companyAnswers
                )

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
              val journeyDataReturnFound = retrievedJourneyDataWithSaStatus(SAStatus.ReturnFound)
              val session                =
                Fixtures.individualHECSession(individualWithSautr, journeyDataReturnFound, individualAnswers)
              val updatedSession         =
                Fixtures.individualHECSession(
                  individualWithSautr,
                  journeyDataReturnFound,
                  answersWithTaxSituation(taxSituation)
                )

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
              val journeyDataNoReturnFound = retrievedJourneyDataWithSaStatus(SAStatus.NoReturnFound)
              val session                  =
                Fixtures.individualHECSession(individualWithSautr, journeyDataNoReturnFound, individualAnswers)
              val updatedSession           =
                Fixtures.individualHECSession(
                  individualWithSautr,
                  journeyDataNoReturnFound,
                  answersWithTaxSituation(taxSituation)
                )

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
              val journeyDataNoticeIssued = retrievedJourneyDataWithSaStatus(SAStatus.NoticeToFileIssued)
              val session                 =
                Fixtures.individualHECSession(individualWithSautr, journeyDataNoticeIssued, individualAnswers)
              val updatedSession          =
                Fixtures.individualHECSession(
                  individualWithSautr,
                  journeyDataNoticeIssued,
                  answersWithTaxSituation(taxSituation)
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

          "tax situation is SA" when {
            testsForSelfAssessment(TaxSituation.SA)
          }

          "tax situation is SAPAYE" when {
            testsForSelfAssessment(TaxSituation.SAPAYE)
          }
        }

        "the SA income statement page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.SAController.saIncomeStatement(),
            session
          )
          await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
        }

        "the check your answers page" when {

          "all user answers are not complete" in {
            val session                                     = IndividualHECSession.newSession(individualLoginData)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException](
              await(
                journeyService
                  .updateAndNext(
                    routes.CheckYourAnswersController.checkYourAnswers(),
                    session
                  )
                  .value
              )
            )

          }

          "all user answers are complete" in {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              None,
              Some(taxCheckStartDateTime)
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers(),
              session
            )
            await(result.value) shouldBe Right(routes.TaxCheckCompleteController.taxCheckComplete())

          }

        }

        "the company registration number page" when {

          def testCrnNextpage(companyName: Option[CompanyHouseName], resultCall: Call) = {
            val session        = CompanyHECSession.newSession(companyLoginData)
            val updatedSession =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty.copy(companyName = companyName),
                CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567")))
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

            val session                                     = CompanyHECSession.newSession(companyLoginData)
            val updatedSession                              =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567")))
              )
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[RuntimeException](
              await(
                journeyService
                  .updateAndNext(
                    routes.CRNController.companyRegistrationNumber(),
                    updatedSession
                  )
                  .value
              )
            )

          }

        }

        "the confirm company name page" when {

          "the user says company details are wrong" in {
            val journeyData =
              CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test tech Ltd")))

            val session        = Fixtures.companyHECSession(
              companyLoginData,
              journeyData,
              CompanyUserAnswers.empty
            )
            val updatedSession =
              Fixtures.companyHECSession(
                companyLoginData,
                journeyData,
                CompanyUserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.No)
                )
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CRNController.companyRegistrationNumber())
          }

          "the user answer for confirmation of company details is missing" in {
            val journeyData =
              CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test tech Ltd")))

            val session        = Fixtures.companyHECSession(
              companyLoginData,
              journeyData,
              CompanyUserAnswers.empty
            )
            val updatedSession =
              Fixtures.companyHECSession(
                companyLoginData,
                journeyData,
                CompanyUserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = None
                )
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                updatedSession
              )
            }
          }

          "the applicant is an individual" in {
            val session        = IndividualHECSession.newSession(individualLoginData)
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                updatedSession
              )
            }
          }

          def buildSessions(loginData: CompanyLoginData, retrievedJourneyData: CompanyRetrievedJourneyData) = {
            val session        = Fixtures.companyHECSession(loginData, retrievedJourneyData, CompanyUserAnswers.empty)
            val updatedSession =
              Fixtures.companyHECSession(
                loginData,
                retrievedJourneyData,
                CompanyUserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                )
              )
            (session, updatedSession)
          }

          val anyCTStatusResponse                          = CTStatusResponse(
            ctutr = CTUTR("utr"),
            startDate = LocalDate.now(),
            endDate = LocalDate.now(),
            latestAccountingPeriod = None
          )
          def ctStatusResponseWithStatus(status: CTStatus) =
            anyCTStatusResponse.copy(latestAccountingPeriod =
              Some(
                CTAccountingPeriod(
                  startDate = LocalDate.now(),
                  endDate = LocalDate.now(),
                  ctStatus = status
                )
              )
            )

          "enrolments and DES CTUTRs do not match" in {
            val companyData    = companyLoginData.copy(
              ctutr = Some(CTUTR("enrolments-ctutr"))
            )
            val journeyData    = CompanyRetrievedJourneyData.empty.copy(
              companyName = Some(CompanyHouseName("Test tech Ltd")),
              desCtutr = Some(CTUTR("des-ctutr")),
              ctStatus = None
            )
            val session        = Fixtures.companyHECSession(companyData, journeyData, CompanyUserAnswers.empty)
            val updatedSession =
              Fixtures.companyHECSession(
                companyData,
                journeyData,
                CompanyUserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                )
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.ctutrNotMatched())
          }

          "DES CTUTR could not be fetched" in {
            val companyData    = companyLoginData.copy(
              ctutr = Some(CTUTR("enrolments-ctutr"))
            )
            val journeyData    = CompanyRetrievedJourneyData(
              companyName = Some(CompanyHouseName("Test tech Ltd")),
              desCtutr = None,
              ctStatus = None
            )
            val session        = Fixtures.companyHECSession(companyData, journeyData, CompanyUserAnswers.empty)
            val updatedSession =
              Fixtures.companyHECSession(
                companyData,
                journeyData,
                CompanyUserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                )
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck())
          }

          "enrolments and DES CTUTRs match" when {
            def testForCTStatus(status: CTStatus) = {
              val companyData = companyLoginData.copy(
                ctutr = Some(CTUTR("ctutr"))
              )
              val journeyData = CompanyRetrievedJourneyData(
                companyName = Some(CompanyHouseName("Test tech Ltd")),
                desCtutr = Some(CTUTR("ctutr")),
                ctStatus = Some(ctStatusResponseWithStatus(status))
              )

              val (session, updatedSession) = buildSessions(companyData, journeyData)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
              mockStoreSession(updatedSession)(Right(()))

              val result = journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.chargeableForCorporationTax())
            }

            "status = ReturnFound" in {
              testForCTStatus(CTStatus.ReturnFound)
            }

            "status = NoticeToFileIssued" in {
              testForCTStatus(CTStatus.NoticeToFileIssued)
            }

            "status = NoReturnFound" in {
              testForCTStatus(CTStatus.NoReturnFound)
            }

            "no accounting periods found" in {
              val companyData               = companyLoginData.copy(
                ctutr = Some(CTUTR("ctutr"))
              )
              val journeyData               = CompanyRetrievedJourneyData(
                companyName = Some(CompanyHouseName("Test tech Ltd")),
                desCtutr = Some(CTUTR("ctutr")),
                ctStatus = Some(anyCTStatusResponse)
              )
              val (session, updatedSession) = buildSessions(companyData, journeyData)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
              mockStoreSession(updatedSession)(Right(()))

              val result = journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.recentlyStartedTrading())
            }

            "CT status could not be fetched" in {
              val companyData = companyLoginData.copy(
                ctutr = Some(CTUTR("ctutr"))
              )
              val journeyData = CompanyRetrievedJourneyData(
                companyName = Some(CompanyHouseName("Test tech Ltd")),
                desCtutr = Some(CTUTR("ctutr")),
                ctStatus = None
              )

              val session        = Fixtures.companyHECSession(companyData, journeyData, CompanyUserAnswers.empty)
              val updatedSession =
                Fixtures.companyHECSession(
                  companyData,
                  journeyData,
                  CompanyUserAnswers.empty.copy(
                    crn = Some(CRN("1234567")),
                    companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                  )
                )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
              mockStoreSession(updatedSession)(Right(()))

              val result = journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck())
            }

          }

          "no CTUTR found in enrolments but des CTUTR is there" in {
            val companyData = companyLoginData.copy(
              ctutr = None
            )
            val journeyData = CompanyRetrievedJourneyData(
              companyName = Some(CompanyHouseName("Test tech Ltd")),
              desCtutr = Some(CTUTR("ctutr")),
              ctStatus = Some(anyCTStatusResponse)
            )

            val (session, updatedSession) = buildSessions(companyData, journeyData)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.enterCtutr())
          }

          "no CTUTR found in enrolments and  des CTUTR is not found" in {
            val companyData = companyLoginData.copy(
              ctutr = None
            )
            val journeyData = CompanyRetrievedJourneyData(
              companyName = Some(CompanyHouseName("Test tech Ltd")),
              desCtutr = None,
              ctStatus = Some(anyCTStatusResponse)
            )

            val (session, updatedSession) = buildSessions(companyData, journeyData)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck())
          }

        }

        "chargeable for CT page" should {
          val chargeableForCorporationTaxRoute = routes.CompanyDetailsController.chargeableForCorporationTax()
          val date                             = LocalDate.now

          "throw" when {
            "the applicant is an individual" in {
              val session        = IndividualHECSession.newSession(individualLoginData)
              val updatedSession = session.copy(userAnswers = IndividualUserAnswers.empty)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              }
            }

            "applicant is company but chargeable for CT answer is missing" in {
              val session        = CompanyHECSession.newSession(companyLoginData)
              val updatedSession = session.copy(userAnswers = CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567"))))

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              }
            }

            "CT status latest accounting period is missing when user has said they are chargeable for CT" in {
              val date           = LocalDate.now
              val journeyData    = CompanyRetrievedJourneyData.empty.copy(
                ctStatus = Some(CTStatusResponse(CTUTR("utr"), date, date, None))
              )
              val session        = Fixtures.companyHECSession(companyLoginData, journeyData, CompanyUserAnswers.empty)
              val updatedSession = session.copy(
                userAnswers =
                  CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567")), chargeableForCT = Some(YesNoAnswer.Yes))
              )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              }
            }
          }

          "chargeable for CT answer is No" in {
            List(
              CTStatus.ReturnFound,
              CTStatus.NoReturnFound,
              CTStatus.NoticeToFileIssued
            ).foreach { status =>
              withClue(s"for $status") {
                val journeyData    = CompanyRetrievedJourneyData.empty.copy(
                  ctStatus =
                    Some(CTStatusResponse(CTUTR("utr"), date, date, Some(CTAccountingPeriod(date, date, status))))
                )
                val session        =
                  Fixtures.companyHECSession(companyLoginData, journeyData, CompanyUserAnswers.empty)
                val updatedSession = session.copy(
                  userAnswers =
                    CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567")), chargeableForCT = Some(YesNoAnswer.No))
                )

                implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
                mockStoreSession(updatedSession)(Right(()))

                val result = journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
                await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
              }
            }
          }

          "chargeable for CT answer is Yes" when {

            def companyData(status: CTStatus) = CompanyRetrievedJourneyData(
              None,
              None,
              Some(
                CTStatusResponse(
                  CTUTR("utr"),
                  date,
                  date,
                  Some(CTAccountingPeriod(date, date, status))
                )
              )
            )

            val yesUserAnswers = CompanyUserAnswers.empty.copy(
              crn = Some(CRN("1234567")),
              chargeableForCT = Some(YesNoAnswer.Yes)
            )

            def test(status: CTStatus, destination: Call) = {
              val session        =
                Fixtures.companyHECSession(companyLoginData, companyData(status), CompanyUserAnswers.empty)
              val updatedSession = session.copy(userAnswers = yesUserAnswers)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
              mockStoreSession(updatedSession)(Right(()))

              val result = journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              await(result.value) shouldBe Right(destination)
            }

            "status = NoticeToFileIssued" in {
              test(
                status = CTStatus.NoticeToFileIssued,
                destination = routes.CheckYourAnswersController.checkYourAnswers()
              )
            }

            "status = ReturnFound" in {
              test(
                status = CTStatus.ReturnFound,
                destination = routes.CompanyDetailsController.ctIncomeStatement()
              )
            }

            "status = NoReturnFound" in {
              test(
                status = CTStatus.NoReturnFound,
                destination = routes.CompanyDetailsController.cannotDoTaxCheck()
              )
            }
          }

        }

        "CT income statement page" in {
          val session                                     = CompanyHECSession.newSession(companyLoginData)
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.CompanyDetailsController.ctIncomeStatement(),
            session
          )
          await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers())
        }

        "Recently started Trading page" when {

          "the applicant is an individual" in {
            val session        = IndividualHECSession.newSession(individualLoginData)
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            assertThrows[RuntimeException] {
              journeyService.updateAndNext(
                routes.CompanyDetailsController.recentlyStartedTrading(),
                updatedSession
              )
            }
          }

          def testStartTrading(answer: YesNoAnswer, nextCall: Call) = {
            val companyRetrievedJourneyData = CompanyRetrievedJourneyData(
              Some(CompanyHouseName("Test Tech Ltd")),
              Some(CTUTR("4444444444")),
              None
            )

            val updatedUserAnswer = CompanyUserAnswers.empty.copy(
              crn = Some(CRN("1412345")),
              recentlyStartedTrading = Some(answer)
            )

            val session        =
              Fixtures.companyHECSession(
                companyLoginData,
                companyRetrievedJourneyData,
                CompanyUserAnswers.empty
              )
            val updatedSession = session.copy(userAnswers = updatedUserAnswer)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result =
              journeyService.updateAndNext(routes.CompanyDetailsController.recentlyStartedTrading(), updatedSession)
            await(result.value) shouldBe Right(nextCall)
          }

          "applicant select yes" in {
            testStartTrading(YesNoAnswer.Yes, routes.CheckYourAnswersController.checkYourAnswers())
          }

          "applicant select no" in {
            testStartTrading(YesNoAnswer.No, routes.CompanyDetailsController.cannotDoTaxCheck())
          }

        }

        "enter CTUTR page" when {

          "number of attempts has reached the maximum & no valid CTUTR was found" in {
            val session = Fixtures.companyHECSession(crnBlocked = true)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.enterCtutr(),
              session
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.tooManyCtutrAttempts())
          }

          "number of attempts has not reached the maximum" must {

            "throw error if CTUTR is missing" in {
              val session = Fixtures.companyHECSession()

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(
                  routes.CompanyDetailsController.enterCtutr(),
                  session
                )
              }
            }

            "throw error if DES CTUTR is missing" in {
              val session = Fixtures.companyHECSession(
                retrievedJourneyData = Fixtures.companyRetrievedJourneyData(
                  ctStatus = Some(Fixtures.ctStatusResponse()),
                  desCtutr = None
                )
              )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(
                  routes.CompanyDetailsController.enterCtutr(),
                  session
                )
              }
            }

            "throw error if CTUTR answer is not found" in {
              val session = Fixtures.companyHECSession(
                retrievedJourneyData = Fixtures.companyRetrievedJourneyData(
                  ctStatus = None,
                  desCtutr = Some(CTUTR("utr"))
                ),
                userAnswers = Fixtures.incompleteCompanyUserAnswers(ctutr = None)
              )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(
                  routes.CompanyDetailsController.enterCtutr(),
                  session
                )
              }
            }

            "go to cannot do tax check page when CT status is not found" in {
              val session = Fixtures.companyHECSession(
                retrievedJourneyData = Fixtures.companyRetrievedJourneyData(
                  ctStatus = None,
                  desCtutr = Some(CTUTR("utr"))
                ),
                userAnswers = Fixtures.incompleteCompanyUserAnswers(ctutr = Some(CTUTR("utr")))
              )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              val result = journeyService.updateAndNext(
                routes.CompanyDetailsController.enterCtutr(),
                session
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck())
            }

            "go to recently started trading page when no latest accounting period found" in {
              val session = Fixtures.companyHECSession(
                retrievedJourneyData = Fixtures.companyRetrievedJourneyData(
                  ctStatus = Some(Fixtures.ctStatusResponse(latestAccountingPeriod = None)),
                  desCtutr = Some(CTUTR("utr"))
                ),
                userAnswers = Fixtures.incompleteCompanyUserAnswers(ctutr = Some(CTUTR("utr")))
              )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              val result = journeyService.updateAndNext(
                routes.CompanyDetailsController.enterCtutr(),
                session
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.recentlyStartedTrading())
            }

            "go to chargeable for CT page when latest accounting period found" in {
              val session = Fixtures.companyHECSession(
                retrievedJourneyData = Fixtures.companyRetrievedJourneyData(
                  ctStatus =
                    Some(Fixtures.ctStatusResponse(latestAccountingPeriod = Some(Fixtures.ctAccountingPeriod()))),
                  desCtutr = Some(CTUTR("utr"))
                ),
                userAnswers = Fixtures.incompleteCompanyUserAnswers(ctutr = Some(CTUTR("utr")))
              )

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              val result = journeyService.updateAndNext(
                routes.CompanyDetailsController.enterCtutr(),
                session
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.chargeableForCorporationTax())
            }
          }
        }

      }

      "convert incomplete answers to complete answers when all questions have been answered and" when {

        "user is an individual and " when {

          "the user has selected an individual only licence type" in {
            val completeAnswers = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.PAYE
            )

            val incompleteAnswers = Fixtures.incompleteIndividualUserAnswers(
              Some(completeAnswers.licenceType),
              Some(completeAnswers.licenceTimeTrading),
              Some(completeAnswers.licenceValidityPeriod),
              Some(completeAnswers.taxSituation),
              completeAnswers.saIncomeDeclared
            )

            val individualData = individualLoginData.copy(sautr = Some(SAUTR("utr")))
            val journeyData    =
              IndividualRetrievedJourneyData(
                saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoticeToFileIssued))
              )

            val session = Fixtures.individualHECSession(individualData, journeyData, incompleteAnswers)

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
                val completeAnswers = Fixtures.completeIndividualUserAnswers(
                  licenceType,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToOneYear,
                  TaxSituation.PAYE,
                  Some(YesNoAnswer.Yes),
                  Some(EntityType.Company)
                )

                val incompleteAnswers = Fixtures.incompleteIndividualUserAnswers(
                  Some(completeAnswers.licenceType),
                  Some(completeAnswers.licenceTimeTrading),
                  Some(completeAnswers.licenceValidityPeriod),
                  Some(completeAnswers.taxSituation),
                  Some(YesNoAnswer.Yes),
                  Some(EntityType.Company)
                )

                val session                                     =
                  Fixtures.individualHECSession(
                    individualLoginData,
                    IndividualRetrievedJourneyData.empty,
                    incompleteAnswers
                  )
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
                val completeAnswers = Fixtures.completeIndividualUserAnswers(
                  LicenceType.DriverOfTaxisAndPrivateHires,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToOneYear,
                  taxSituation
                )

                val incompleteAnswers = Fixtures.incompleteIndividualUserAnswers(
                  Some(completeAnswers.licenceType),
                  Some(completeAnswers.licenceTimeTrading),
                  Some(completeAnswers.licenceValidityPeriod),
                  Some(completeAnswers.taxSituation),
                  completeAnswers.saIncomeDeclared,
                  completeAnswers.entityType
                )

                val session = Fixtures.individualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  incompleteAnswers
                )

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
                val completeAnswers = Fixtures.completeIndividualUserAnswers(
                  LicenceType.DriverOfTaxisAndPrivateHires,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToOneYear,
                  taxSituation,
                  Some(YesNoAnswer.Yes)
                )

                val incompleteAnswers = Fixtures.incompleteIndividualUserAnswers(
                  Some(completeAnswers.licenceType),
                  Some(completeAnswers.licenceTimeTrading),
                  Some(completeAnswers.licenceValidityPeriod),
                  Some(completeAnswers.taxSituation),
                  completeAnswers.saIncomeDeclared,
                  completeAnswers.entityType
                )

                val journeyData = IndividualRetrievedJourneyData(
                  saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
                )
                val session     =
                  IndividualHECSession(individualLoginData, journeyData, incompleteAnswers, None, None, List.empty)

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
                val completeAnswers = Fixtures.completeIndividualUserAnswers(
                  LicenceType.DriverOfTaxisAndPrivateHires,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToOneYear,
                  taxSituation
                )

                val incompleteAnswers = Fixtures.incompleteIndividualUserAnswers(
                  Some(completeAnswers.licenceType),
                  Some(completeAnswers.licenceTimeTrading),
                  Some(completeAnswers.licenceValidityPeriod),
                  Some(completeAnswers.taxSituation),
                  completeAnswers.saIncomeDeclared,
                  completeAnswers.entityType
                )

                val journeyData = IndividualRetrievedJourneyData(
                  saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoReturnFound))
                )
                val session     =
                  IndividualHECSession(individualLoginData, journeyData, incompleteAnswers, None, None, List.empty)

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

        "user is a company and " when {

          val startDate = LocalDate.of(2020, 10, 9)
          val endDate   = LocalDate.of(2021, 10, 9)

          def createCTStatus(latestAccountingPeriod: Option[CTAccountingPeriod]) =
            CTStatusResponse(
              ctutr = CTUTR("1111111111"),
              startDate = LocalDate.of(2020, 10, 9),
              endDate = LocalDate.of(2021, 10, 9),
              latestAccountingPeriod = latestAccountingPeriod
            )
          def getCompanySessionData(
            licenceType: LicenceType,
            chargeableForCTOpt: Option[YesNoAnswer],
            ctIncomeDeclaredOpt: Option[YesNoAnswer],
            recentlyStartedTradingOpt: Option[YesNoAnswer],
            ctStatusResponse: Option[CTStatusResponse]
          ): (CompleteCompanyUserAnswers, CompanyHECSession) = {

            val completeAnswers   = Fixtures.completeCompanyUserAnswers(
              licenceType,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              entityType = EntityType.Company,
              crn = CRN("1123456"),
              companyDetailsConfirmed = YesNoAnswer.Yes,
              chargeableForCT = chargeableForCTOpt,
              ctIncomeDeclared = ctIncomeDeclaredOpt,
              recentlyStartedTrading = recentlyStartedTradingOpt
            )
            val incompleteAnswers = Fixtures.incompleteCompanyUserAnswers(
              Some(completeAnswers.licenceType),
              Some(completeAnswers.licenceTimeTrading),
              Some(completeAnswers.licenceValidityPeriod),
              entityType = Some(EntityType.Company),
              crn = Some(CRN("1123456")),
              companyDetailsConfirmed = Some(YesNoAnswer.Yes),
              chargeableForCT = chargeableForCTOpt,
              ctIncomeDeclared = ctIncomeDeclaredOpt,
              recentlyStartedTrading = recentlyStartedTradingOpt
            )

            val session =
              CompanyHECSession(
                Fixtures.companyLoginData(ctutr = Some(CTUTR("1111111111"))),
                Fixtures.companyRetrievedJourneyData(ctStatus = ctStatusResponse),
                incompleteAnswers,
                None,
                None,
                List.empty
              )
            (completeAnswers, session)

          }

          def nextPageIsCYA(session: CompanyHECSession, completeAnswers: CompanyUserAnswers) = {
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

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

                val (completeAnswers, session) = getCompanySessionData(
                  licenceType,
                  Some(YesNoAnswer.Yes),
                  Some(YesNoAnswer.Yes),
                  Some(YesNoAnswer.Yes),
                  Some(createCTStatus(Some(CTAccountingPeriod(startDate, endDate, CTStatus.ReturnFound))))
                )
                nextPageIsCYA(session, completeAnswers)

              }

            }

          }

          "the user has some ctStatus Response with latest accounting period status None, recentlyStartedTrading has Yes value " in {
            val (completeAnswers, session) = getCompanySessionData(
              LicenceType.ScrapMetalMobileCollector,
              recentlyStartedTradingOpt = Some(YesNoAnswer.Yes),
              ctStatusResponse = Some(createCTStatus(None)),
              ctIncomeDeclaredOpt = None,
              chargeableForCTOpt = None
            )
            nextPageIsCYA(session, completeAnswers)

          }

          "the user has some ctStatus Response with latest accounting period status NoticeToFileIssued, chargeableForCT has some value " in {
            List(
              YesNoAnswer.Yes,
              YesNoAnswer.No
            ).foreach { yesNo =>
              val (completeAnswers, session) = getCompanySessionData(
                LicenceType.ScrapMetalMobileCollector,
                recentlyStartedTradingOpt = None,
                ctStatusResponse =
                  Some(createCTStatus(Some(CTAccountingPeriod(startDate, endDate, CTStatus.NoticeToFileIssued)))),
                ctIncomeDeclaredOpt = None,
                chargeableForCTOpt = Some(yesNo)
              )
              nextPageIsCYA(session, completeAnswers)
            }
          }

          "the user has some ctStatus Response with latest accounting period status NoReturnFound, chargeableForCT has No value " in {
            val (completeAnswers, session) = getCompanySessionData(
              LicenceType.ScrapMetalMobileCollector,
              recentlyStartedTradingOpt = None,
              ctStatusResponse =
                Some(createCTStatus(Some(CTAccountingPeriod(startDate, endDate, CTStatus.NoReturnFound)))),
              ctIncomeDeclaredOpt = None,
              chargeableForCTOpt = Some(YesNoAnswer.No)
            )
            nextPageIsCYA(session, completeAnswers)
          }

        }

      }

      "not convert incomplete answers to complete answers when all questions have been answered and" when {

        "the selected entity type is not consistent with the entity type retrieved from the GG creds" in {
          val incompleteAnswers = Fixtures.incompleteIndividualUserAnswers(
            Some(LicenceType.OperatorOfPrivateHireVehicles),
            Some(LicenceTimeTrading.ZeroToTwoYears),
            Some(LicenceValidityPeriod.UpToOneYear),
            Some(TaxSituation.PAYE),
            Some(YesNoAnswer.Yes),
            Some(EntityType.Company)
          )

          val session                                     =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              incompleteAnswers
            )
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
          val session                                     = CompanyHECSession.newSession(companyLoginData)
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
          val session                                     = CompanyHECSession.newSession(companyLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.StartController.start()
          )

          result shouldBe routes.StartController.start()
        }

        "the confirm individual details page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
          )

          result shouldBe routes.StartController.start()
        }

        "the confirm individual details exit page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit()
          )

          result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
        }

        "the tax check codes page" when {
          val taxChecks = List(
            TaxCheckListItem(
              LicenceType.DriverOfTaxisAndPrivateHires,
              HECTaxCheckCode("some-code"),
              LocalDate.now(),
              ZonedDateTime.now()
            )
          )

          "applicant is individual" in {
            val session = IndividualHECSession.newSession(individualLoginData).copy(unexpiredTaxChecks = taxChecks)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(routes.TaxChecksListController.unexpiredTaxChecks())
            result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
          }

          "applicant is company" in {
            val session = CompanyHECSession.newSession(companyLoginData).copy(unexpiredTaxChecks = taxChecks)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(routes.TaxChecksListController.unexpiredTaxChecks())
            result shouldBe routes.StartController.start()
          }
        }

        "the licence type page" when afterWord("the user is") {

          val taxChecks = List(
            TaxCheckListItem(
              LicenceType.DriverOfTaxisAndPrivateHires,
              HECTaxCheckCode("some-code"),
              LocalDate.now(),
              ZonedDateTime.now()
            )
          )

          "an individual" when {
            "there are preexisting tax check codes" in {
              val session                                     = Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty,
                unexpiredTaxChecks = taxChecks
              )
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType()
              )

              result shouldBe routes.TaxChecksListController.unexpiredTaxChecks()
            }

            "there are no preexisting tax check codes" in {
              val session                                     = IndividualHECSession.newSession(individualLoginData)
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType()
              )

              result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails()
            }
          }

          "a company" when {
            "there are preexisting tax check codes" in {
              val session                                     = Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                CompanyUserAnswers.empty,
                unexpiredTaxChecks = taxChecks
              )
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType()
              )

              result shouldBe routes.TaxChecksListController.unexpiredTaxChecks()
            }

            "there are no preexisting tax check codes" in {
              val session                                     = CompanyHECSession.newSession(companyLoginData)
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType()
              )

              result shouldBe routes.StartController.start()
            }
          }

        }

        "the licence type exit page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceTypeExit()
          )

          result shouldBe routes.LicenceDetailsController.licenceType()
        }

        "the licence time trading page when the session contains an licence expiry date which is not more than 1 year ago" in {
          val session                                     = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(
              licenceType = Some(LicenceType.ScrapMetalDealerSite)
            )
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
                val session                                     = Fixtures.individualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  IndividualUserAnswers.empty.copy(
                    licenceType = Some(licenceType)
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
          val session                                     = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
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
          val session                                     = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
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

            val session                                     = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
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
            val session                                     = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
                licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
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

        "the check your answers page" when {

          def testPrevPageIsTaxSituation(session: HECSession) = {
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.CheckYourAnswersController.checkYourAnswers()
            )

            result shouldBe routes.TaxSituationController.taxSituation()
          }

          val individualWithSautr =
            individualLoginData.copy(sautr = Some(SAUTR("utr")))

          val journeyDataWithSaStatus = IndividualRetrievedJourneyData(saStatus =
            Some(SAStatusResponse(SAUTR(""), TaxYear(2020), SAStatus.NoticeToFileIssued))
          )

          def userAnswers(licenceType: LicenceType, taxSituation: TaxSituation, entityType: Option[EntityType]) =
            Fixtures.completeIndividualUserAnswers(
              licenceType,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              taxSituation,
              Some(YesNoAnswer.Yes),
              entityType
            )

          "tax situation = SA & SA status = NoticeToFileIssued" in {
            val session = IndividualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(LicenceType.DriverOfTaxisAndPrivateHires, TaxSituation.SA, None),
              None,
              None,
              List.empty
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = SAPAYE & SA status = NoticeToFileIssued" in {
            val session = IndividualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(LicenceType.DriverOfTaxisAndPrivateHires, TaxSituation.SAPAYE, None),
              None,
              None,
              List.empty
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = PAYE" in {
            val session = IndividualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                TaxSituation.PAYE,
                None
              ),
              None,
              None,
              List.empty
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = Not Chargeable" in {
            val session = IndividualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                TaxSituation.NotChargeable,
                None
              ),
              None,
              None,
              List.empty
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
          val session                                     = Fixtures.companyHECSession(
            companyLoginData,
            CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test Tech Ltd"))),
            CompanyUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            )
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CRNController.companyRegistrationNumber()
          )

          result shouldBe routes.EntityTypeController.entityType
        }

        "the confirm company details page" in {
          val session                                     = Fixtures.companyHECSession(
            companyLoginData,
            CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test Tech Ltd"))),
            CompanyUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            )
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CompanyDetailsController.confirmCompanyDetails()
          )

          result shouldBe routes.CRNController.companyRegistrationNumber()

        }

        "the recently started trading page" in {

          val companyData         = companyLoginData.copy(
            ctutr = Some(CTUTR("ctutr"))
          )
          val anyCTStatusResponse = CTStatusResponse(
            ctutr = CTUTR("utr"),
            startDate = LocalDate.now(),
            endDate = LocalDate.now(),
            latestAccountingPeriod = None
          )
          val journeyData         = CompanyRetrievedJourneyData(
            companyName = Some(CompanyHouseName("Test tech Ltd")),
            desCtutr = Some(CTUTR("ctutr")),
            ctStatus = Some(anyCTStatusResponse)
          )

          val updatedSession =
            Fixtures.companyHECSession(
              companyData,
              journeyData,
              CompanyUserAnswers.empty.copy(
                licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
                licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears),
                licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
                entityType = Some(Company),
                crn = Some(CRN("1234567")),
                companyDetailsConfirmed = Some(YesNoAnswer.Yes)
              )
            )

          implicit val request: RequestWithSessionData[_] = requestWithSessionData(updatedSession)
          val result                                      = journeyService.previous(
            routes.CompanyDetailsController.recentlyStartedTrading()
          )

          result shouldBe routes.CompanyDetailsController.confirmCompanyDetails()
        }

        "enter CTUTR page" in {
          val companyData = companyLoginData.copy(
            ctutr = None
          )

          val anyCTStatusResponse = CTStatusResponse(
            ctutr = CTUTR("utr"),
            startDate = LocalDate.now(),
            endDate = LocalDate.now(),
            latestAccountingPeriod = None
          )
          val journeyData         = CompanyRetrievedJourneyData(
            companyName = Some(CompanyHouseName("Test tech Ltd")),
            desCtutr = Some(CTUTR("ctutr")),
            ctStatus = Some(anyCTStatusResponse)
          )

          val updatedSession =
            Fixtures.companyHECSession(
              companyData,
              journeyData,
              CompanyUserAnswers.empty.copy(
                licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
                licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears),
                licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
                entityType = Some(Company),
                crn = Some(CRN("1234567")),
                companyDetailsConfirmed = Some(YesNoAnswer.Yes)
              )
            )

          implicit val request: RequestWithSessionData[_] = requestWithSessionData(updatedSession)
          val result                                      = journeyService.previous(
            routes.CompanyDetailsController.enterCtutr()
          )

          result shouldBe routes.CompanyDetailsController.confirmCompanyDetails()

        }

        "the 'dont have' CTUTR' page" in {
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(Fixtures.companyHECSession())
          val result                                      = journeyService.previous(
            routes.CompanyDetailsController.dontHaveUtr()
          )

          result shouldBe routes.CompanyDetailsController.enterCtutr()
        }

        "the 'too many CTUTR attempts' page" in {
          val companyData = companyLoginData.copy(
            ctutr = None
          )

          val ctStatusResponse = CTStatusResponse(
            ctutr = CTUTR("utr"),
            startDate = LocalDate.now(),
            endDate = LocalDate.now(),
            latestAccountingPeriod = None
          )
          val journeyData      = CompanyRetrievedJourneyData(
            companyName = Some(CompanyHouseName("Test tech Ltd")),
            desCtutr = Some(CTUTR("ctutr")),
            ctStatus = Some(ctStatusResponse)
          )
          val session          =
            Fixtures.companyHECSession(
              companyData,
              journeyData,
              CompanyUserAnswers.empty.copy(
                licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
                licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears),
                licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
                entityType = Some(Company),
                crn = Some(CRN("1234567")),
                companyDetailsConfirmed = Some(YesNoAnswer.Yes)
              ),
              crnBlocked = true
            )

          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
          val result                                      = journeyService.previous(
            routes.CompanyDetailsController.tooManyCtutrAttempts()
          )

          result shouldBe routes.CompanyDetailsController.enterCtutr()
        }

        def buildIndividualSession(taxSituation: TaxSituation, saStatus: SAStatus): HECSession = {
          val individualLoginData =
            IndividualLoginData(
              GGCredId(""),
              NINO(""),
              Some(SAUTR("utr")),
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None
            )

          val journeyData =
            IndividualRetrievedJourneyData(saStatus = Some(SAStatusResponse(SAUTR(""), TaxYear(2020), saStatus)))

          IndividualHECSession(
            individualLoginData,
            journeyData,
            Fixtures.incompleteIndividualUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToOneYear),
              Some(taxSituation),
              Some(YesNoAnswer.Yes)
            ),
            None,
            None,
            List.empty
          )
        }

      }

      "return the check your answers page" when {

        "the answers in the session are complete and the current page is not the check your answers page" in {
          val completeAnswers                             = Fixtures.completeIndividualUserAnswers(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            TaxSituation.PAYE,
            Some(YesNoAnswer.Yes),
            Some(EntityType.Individual)
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                completeAnswers,
                None,
                None,
                List.empty
              )
            )

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceType()
          )

          result shouldBe routes.CheckYourAnswersController.checkYourAnswers()
        }

      }

    }

    "JourneyServiceImpl.allIndividualAnswersComplete" must {
      val incompleteAnswersBase = Fixtures.incompleteIndividualUserAnswers(
        licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
        licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears),
        licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
        taxSituation = Some(TaxSituation.PAYE)
      )

      "return false" when {

        "licence type is missing" in {
          JourneyServiceImpl.allIndividualAnswersComplete(
            incompleteAnswersBase.copy(licenceType = None),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "licence time trading is missing" in {
          JourneyServiceImpl.allIndividualAnswersComplete(
            incompleteAnswersBase.copy(licenceTimeTrading = None),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "licence validity period is missing" in {
          JourneyServiceImpl.allIndividualAnswersComplete(
            incompleteAnswersBase.copy(licenceValidityPeriod = None),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "entity type is present" in {
          JourneyServiceImpl.allIndividualAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase.copy(entityType = Some(EntityType.Individual)),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "tax situation = SA & status = ReturnFound & income declared is missing)" in {
          List(
            TaxSituation.SA,
            TaxSituation.SAPAYE
          ) foreach { taxSituation =>
            withClue(s"for $taxSituation") {
              val journeyData = IndividualRetrievedJourneyData(
                saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )

              val session =
                IndividualHECSession.newSession(individualLoginData).copy(retrievedJourneyData = journeyData)

              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = None
                ),
                session
              ) shouldBe false
            }
          }
        }
      }

      "return true" when {

        "entity type is missing" in {
          JourneyServiceImpl.allIndividualAnswersComplete(
            incompleteAnswersBase,
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe true
        }

        "tax situation = non-SA (irrespective of whether income declared is missing or present)" in {
          List(
            TaxSituation.PAYE,
            TaxSituation.NotChargeable
          ) foreach { taxSituation =>
            withClue(s"for $taxSituation") {
              // income declared is missing
              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = None
                ),
                IndividualHECSession.newSession(individualLoginData)
              ) shouldBe true

              // income declared is present
              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = Some(YesNoAnswer.Yes)
                ),
                IndividualHECSession.newSession(individualLoginData)
              ) shouldBe true
            }
          }
        }

        "tax situation = SA & status != ReturnFound (irrespective of whether income declared is missing or present)" in {
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
              val journeyData =
                IndividualRetrievedJourneyData(
                  saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), saStatus))
                )

              val session =
                IndividualHECSession.newSession(individualLoginData).copy(retrievedJourneyData = journeyData)

              // income declared is missing
              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = None
                ),
                session
              ) shouldBe true

              // income declared is present
              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = Some(YesNoAnswer.Yes)
                ),
                session
              ) shouldBe true
            }
          }
        }

        "tax situation = SA & status = ReturnFound & income declared is present)" in {
          List(
            TaxSituation.SA,
            TaxSituation.SAPAYE
          ) foreach { taxSituation =>
            withClue(s"for $taxSituation") {
              val journeyData = IndividualRetrievedJourneyData(
                saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )

              val session =
                IndividualHECSession.newSession(individualLoginData).copy(retrievedJourneyData = journeyData)

              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = Some(YesNoAnswer.Yes)
                ),
                session
              ) shouldBe true
            }
          }
        }

      }
    }

    "JourneyServiceImpl.allCompanyAnswersComplete" must {
      val incompleteAnswersBase = Fixtures.incompleteCompanyUserAnswers(
        licenceType = Some(LicenceType.ScrapMetalDealerSite),
        licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears),
        licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
        entityType = Some(Company)
      )

      def checkCompanyDataComplete(
        chargeableForCTOpt: Option[YesNoAnswer],
        ctIncomeDeclaredOpt: Option[YesNoAnswer],
        recentlyStartedTradingOpt: Option[YesNoAnswer],
        latestAccountingPeriod: Option[CTAccountingPeriod],
        licenceType: Some[LicenceType] = Some(LicenceType.ScrapMetalDealerSite)
      ) = {
        val date                = LocalDate.now()
        val companyData         = companyLoginData.copy(
          ctutr = Some(CTUTR("ctutr"))
        )
        val anyCTStatusResponse = CTStatusResponse(
          ctutr = CTUTR("utr"),
          startDate = date,
          endDate = date,
          latestAccountingPeriod = latestAccountingPeriod
        )
        val journeyData         = CompanyRetrievedJourneyData(
          companyName = Some(CompanyHouseName("Test tech Ltd")),
          desCtutr = Some(CTUTR("ctutr")),
          ctStatus = Some(anyCTStatusResponse)
        )

        val incompleteAnswers = incompleteAnswersBase.copy(
          licenceType = licenceType,
          entityType = Some(Company),
          crn = Some(CRN("1234567")),
          companyDetailsConfirmed = Some(YesNoAnswer.Yes),
          chargeableForCT = chargeableForCTOpt,
          ctIncomeDeclared = ctIncomeDeclaredOpt,
          recentlyStartedTrading = recentlyStartedTradingOpt
        )
        val session           = Fixtures.companyHECSession(
          companyData,
          journeyData,
          CompanyUserAnswers.empty
        )
        JourneyServiceImpl.allCompanyAnswersComplete(incompleteAnswers, session)
      }

      val date = LocalDate.now()

      "return false" when {
        "licence type is missing" in {
          JourneyServiceImpl.allCompanyAnswersComplete(
            incompleteAnswersBase.copy(licenceType = None),
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "licence time trading is missing" in {
          JourneyServiceImpl.allCompanyAnswersComplete(
            incompleteAnswersBase.copy(licenceTimeTrading = None),
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "licence validity period is missing" in {
          JourneyServiceImpl.allCompanyAnswersComplete(
            incompleteAnswersBase.copy(licenceValidityPeriod = None),
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "entity type is missing" in {
          JourneyServiceImpl.allCompanyAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase,
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "recently started trading is not present " when {

          "chargeable for CT answer is Yes, CT status = ReturnFound & CT income declared answer is missing" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              None,
              Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is Yes & CT status = NoReturnFound" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              None,
              Some(CTAccountingPeriod(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is not present" in {
            checkCompanyDataComplete(
              None,
              None,
              None,
              Some(CTAccountingPeriod(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

        }

        "recently started trading is  present and is NO " when {

          "chargeable for CT answer is Yes, CT status = ReturnFound & CT income declared answer is missing" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              Some(YesNoAnswer.No),
              Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is Yes & CT status = NoReturnFound" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              Some(YesNoAnswer.No),
              Some(CTAccountingPeriod(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is not present" in {
            checkCompanyDataComplete(
              None,
              None,
              Some(YesNoAnswer.No),
              Some(CTAccountingPeriod(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

        }

      }

      "return true" when {

        "recently started trading is not present" when {

          "CT status is present & chargeable for CT answer is No" in {
            val date                                        = LocalDate.now
            val licenceTypes                                = List(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceType.ScrapMetalDealerSite,
              LicenceType.ScrapMetalMobileCollector
            )
            val ctStatuses                                  = List(
              CTStatus.ReturnFound,
              CTStatus.NoReturnFound,
              CTStatus.NoticeToFileIssued
            )
            val permutations: List[(LicenceType, CTStatus)] = for {
              licenceType <- licenceTypes
              ctStatus    <- ctStatuses
            } yield (licenceType, ctStatus)
            permutations foreach { case (licenceType, ctStatus) =>
              withClue(s"for licenceType = $licenceType & CT status = $ctStatus") {
                checkCompanyDataComplete(
                  Some(YesNoAnswer.No),
                  None,
                  None,
                  Some(CTAccountingPeriod(date, date, ctStatus)),
                  Some(licenceType)
                ) shouldBe true
              }
            }
          }

          "chargeable for CT answer is Yes and CT status = NoticeToFileIssued" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              None,
              Some(CTAccountingPeriod(date, date, CTStatus.NoticeToFileIssued))
            ) shouldBe true
          }

          "chargeable for CT answer is Yes, CT status = ReturnFound & CT income declared answer is present" in {

            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              Some(YesNoAnswer.Yes),
              None,
              Some(CTAccountingPeriod(date, date, CTStatus.ReturnFound))
            ) shouldBe true

          }

        }

        "recently started trading is present and  is yes" when {

          "chargeable for CT answer is not present, CT income declared answer is no present" in {
            checkCompanyDataComplete(
              None,
              None,
              Some(YesNoAnswer.Yes),
              None
            ) shouldBe true
          }

        }

      }
    }

  }

}
