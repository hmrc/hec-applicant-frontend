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
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.controllers.{SessionSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.{Company, Individual}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation.PAYE
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod.UpToOneYear
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyServiceSpec extends AnyWordSpec with Matchers with MockFactory with SessionSupport {

  val journeyService: JourneyServiceImpl = new JourneyServiceImpl(mockSessionStore)

  val taxCheckStartDateTime = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))

  def requestWithSessionData(s: HECSession): RequestWithSessionData[_] =
    RequestWithSessionData(AuthenticatedRequest(FakeRequest()), s)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val individualLoginData: IndividualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData: CompanyLoginData =
    CompanyLoginData(GGCredId(""), None, None)

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
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)),
                None,
                None,
                List.empty
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

        "the licence time trading page" when {

          "when all user answers are not complete" in {
            val session        = IndividualHECSession.newSession(individualLoginData)
            val updatedSession =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                UserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.TwoToFourYears)),
                None,
                None,
                List.empty
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
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                CompleteUserAnswers(
                  licenceType = DriverOfTaxisAndPrivateHires,
                  licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                  licenceValidityPeriod = UpToOneYear,
                  taxSituation = Some(PAYE),
                  saIncomeDeclared = Some(YesNoAnswer.Yes),
                  entityType = Some(Individual),
                  crn = None,
                  companyDetailsConfirmed = None,
                  chargeableForCT = None
                ),
                None,
                Some(taxCheckStartDateTime),
                List.empty
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
            val answers        = UserAnswers.empty
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToTwoYears)),
                None,
                None,
                List.empty
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
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToTwoYears)),
                None,
                None,
                List.empty
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
                val session        =
                  IndividualHECSession(
                    individualLoginData,
                    IndividualRetrievedJourneyData.empty,
                    answers,
                    None,
                    None,
                    List.empty
                  )
                val updatedSession =
                  IndividualHECSession(
                    individualLoginData,
                    IndividualRetrievedJourneyData.empty,
                    answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear)),
                    None,
                    None,
                    List.empty
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

            val updatedAnswers = UserAnswers.empty.copy(entityType = Some(selectedEntityType))
            val updatedSession =
              session.fold(
                _.copy(userAnswers = updatedAnswers),
                _.copy(userAnswers = updatedAnswers)
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
            val session                                     =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
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

          val answers = UserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))

          def answersWithTaxSituation(taxSituation: TaxSituation) = UserAnswers.empty.copy(
            licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
            taxSituation = Some(taxSituation)
          )

          "tax situation is missing" in {
            val session        = IndividualHECSession(
              individualWithSautr,
              retrievedJourneyDataWithSaStatus(SAStatus.NoticeToFileIssued),
              answers,
              None,
              None,
              List.empty
            )
            val updatedSession =
              IndividualHECSession(
                individualWithSautr,
                retrievedJourneyDataWithSaStatus(SAStatus.ReturnFound),
                answers,
                None,
                None,
                List.empty
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
              IndividualHECSession(
                individualWithSautr,
                retrievedJourneyDataWithSaStatus(),
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession = IndividualHECSession(
              individualWithSautr,
              retrievedJourneyDataWithSaStatus(),
              answersWithTaxSituation(taxSituation),
              None,
              None,
              List.empty
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
              val answersWithTaxSituation = answers.copy(taxSituation = Some(taxSituation))
              val session                 = IndividualHECSession(
                individualLoginData.copy(sautr = Some(SAUTR(""))),
                IndividualRetrievedJourneyData.empty,
                answersWithTaxSituation,
                None,
                None,
                List.empty
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
              val answersWithTaxSituation = answers.copy(taxSituation = Some(taxSituation))
              val session                 =
                IndividualHECSession(
                  individualWithoutSautr,
                  IndividualRetrievedJourneyData.empty,
                  answers,
                  None,
                  None,
                  List.empty
                )
              val updatedSession          =
                IndividualHECSession(
                  individualWithoutSautr,
                  IndividualRetrievedJourneyData.empty,
                  answersWithTaxSituation,
                  None,
                  None,
                  List.empty
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
                CompanyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers, None, None, List.empty)
              val updatedSession =
                CompanyHECSession(
                  companyLoginData,
                  CompanyRetrievedJourneyData.empty,
                  answersWithTaxSituation(taxSituation),
                  None,
                  None,
                  List.empty
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
                IndividualHECSession(individualWithSautr, journeyDataReturnFound, answers, None, None, List.empty)
              val updatedSession         =
                IndividualHECSession(
                  individualWithSautr,
                  journeyDataReturnFound,
                  answersWithTaxSituation(taxSituation),
                  None,
                  None,
                  List.empty
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
                IndividualHECSession(individualWithSautr, journeyDataNoReturnFound, answers, None, None, List.empty)
              val updatedSession           =
                IndividualHECSession(
                  individualWithSautr,
                  journeyDataNoReturnFound,
                  answersWithTaxSituation(taxSituation),
                  None,
                  None,
                  List.empty
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
                IndividualHECSession(individualWithSautr, journeyDataNoticeIssued, answers, None, None, List.empty)
              val updatedSession          =
                IndividualHECSession(
                  individualWithSautr,
                  journeyDataNoticeIssued,
                  answersWithTaxSituation(taxSituation),
                  None,
                  None,
                  List.empty
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
            val session                                     = IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              CompleteUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = Some(PAYE),
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual),
                crn = None,
                companyDetailsConfirmed = None,
                chargeableForCT = None
              ),
              None,
              Some(taxCheckStartDateTime),
              List.empty
            )
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

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
              CompanyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty.copy(companyName = companyName),
                UserAnswers.empty.copy(crn = Some(CRN("1234567"))),
                None,
                None,
                List.empty
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

        "the confirm company name page" when {

          "the user says company details are wrong" in {
            val journeyData =
              CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test tech Ltd")))

            val session        = CompanyHECSession(
              companyLoginData,
              journeyData,
              UserAnswers.empty,
              None,
              None,
              List.empty
            )
            val updatedSession =
              CompanyHECSession(
                companyLoginData,
                journeyData,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.No)
                ),
                None,
                None,
                List.empty
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

            val session        = CompanyHECSession(
              companyLoginData,
              journeyData,
              UserAnswers.empty,
              None,
              None,
              List.empty
            )
            val updatedSession =
              CompanyHECSession(
                companyLoginData,
                journeyData,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = None
                ),
                None,
                None,
                List.empty
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
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                ),
                None,
                None,
                List.empty
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
            val session        = CompanyHECSession(loginData, retrievedJourneyData, UserAnswers.empty, None, None, List.empty)
            val updatedSession =
              CompanyHECSession(
                loginData,
                retrievedJourneyData,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                ),
                None,
                None,
                List.empty
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
              ctStatus = Some(anyCTStatusResponse)
            )
            val session        = CompanyHECSession(companyData, journeyData, UserAnswers.empty, None, None, List.empty)
            val updatedSession =
              CompanyHECSession(
                companyData,
                journeyData,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                ),
                None,
                None,
                List.empty
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
            val session        = CompanyHECSession(companyData, journeyData, UserAnswers.empty, None, None, List.empty)
            val updatedSession =
              CompanyHECSession(
                companyData,
                journeyData,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                ),
                None,
                None,
                List.empty
              )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.ctutrNotMatched())
          }

          "CT status could not be fetched" in {
            val companyData = companyLoginData.copy(
              ctutr = Some(CTUTR("enrolments-ctutr"))
            )
            val journeyData = CompanyRetrievedJourneyData(
              companyName = Some(CompanyHouseName("Test tech Ltd")),
              desCtutr = Some(CTUTR("des-ctutr")),
              ctStatus = None
            )

            val session        = CompanyHECSession(companyData, journeyData, UserAnswers.empty, None, None, List.empty)
            val updatedSession =
              CompanyHECSession(
                companyData,
                journeyData,
                UserAnswers.empty.copy(
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes)
                ),
                None,
                None,
                List.empty
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
              await(result.value) shouldBe Right(routes.CompanyDetailsController.noAccountingPeriod())
            }
          }

          "no CTUTR found in enrolments" in {
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

        }

        "chargeable for CT page" should {
          val chargeableForCorporationTaxRoute = routes.CompanyDetailsController.chargeableForCorporationTax()
          val date                             = LocalDate.now

          "throw" when {
            "the applicant is an individual" in {
              val session        = IndividualHECSession.newSession(individualLoginData)
              val updatedSession = session.copy(userAnswers = UserAnswers.empty.copy(crn = Some(CRN("1234567"))))

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[RuntimeException] {
                journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              }
            }

            "applicant is company but chargeable for CT answer is missing" in {
              val session        = CompanyHECSession.newSession(companyLoginData)
              val updatedSession = session.copy(userAnswers = UserAnswers.empty.copy(crn = Some(CRN("1234567"))))

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
              val session        = CompanyHECSession(companyLoginData, journeyData, UserAnswers.empty, None, None, List.empty)
              val updatedSession = session.copy(
                userAnswers =
                  UserAnswers.empty.copy(crn = Some(CRN("1234567")), chargeableForCT = Some(YesNoAnswer.Yes))
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
                  CompanyHECSession(companyLoginData, journeyData, UserAnswers.empty, None, None, List.empty)
                val updatedSession = session.copy(
                  userAnswers =
                    UserAnswers.empty.copy(crn = Some(CRN("1234567")), chargeableForCT = Some(YesNoAnswer.No))
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

            val yesUserAnswers = UserAnswers.empty.copy(
              crn = Some(CRN("1234567")),
              chargeableForCT = Some(YesNoAnswer.Yes)
            )

            def test(status: CTStatus, destination: Call) = {
              val session        =
                CompanyHECSession(companyLoginData, companyData(status), UserAnswers.empty, None, None, List.empty)
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
            None,
            None,
            None
          )

          val individualData = individualLoginData.copy(sautr = Some(SAUTR("utr")))
          val journeyData    =
            IndividualRetrievedJourneyData(
              saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoticeToFileIssued))
            )

          val session = IndividualHECSession(individualData, journeyData, incompleteAnswers, None, None, List.empty)

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
                Some(YesNoAnswer.Yes),
                Some(EntityType.Company),
                None,
                None,
                None
              )

              val incompleteAnswers = IncompleteUserAnswers(
                Some(completeAnswers.licenceType),
                Some(completeAnswers.licenceTimeTrading),
                Some(completeAnswers.licenceValidityPeriod),
                completeAnswers.taxSituation,
                Some(YesNoAnswer.Yes),
                Some(EntityType.Company),
                None,
                None,
                None
              )

              val session                                     =
                IndividualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  incompleteAnswers,
                  None,
                  None,
                  List.empty
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
              val completeAnswers = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(taxSituation),
                None,
                None,
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
                None,
                None,
                None
              )

              val session = IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                incompleteAnswers,
                None,
                None,
                List.empty
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
              val completeAnswers = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(taxSituation),
                Some(YesNoAnswer.Yes),
                None,
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
                None,
                None,
                None
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
              val completeAnswers = CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                Some(taxSituation),
                None,
                None,
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
                None,
                None,
                None
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

      "not convert incomplete answers to complete answers when all questions have been answered and" when {

        "the selected entity type is not consistent with the entity type retrieved from the GG creds" in {
          val incompleteAnswers = IncompleteUserAnswers(
            Some(LicenceType.OperatorOfPrivateHireVehicles),
            Some(LicenceTimeTrading.ZeroToTwoYears),
            Some(LicenceValidityPeriod.UpToOneYear),
            Some(TaxSituation.PAYE),
            Some(YesNoAnswer.Yes),
            Some(EntityType.Company),
            None,
            None,
            None
          )

          val session                                     =
            IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              incompleteAnswers,
              None,
              None,
              List.empty
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
              val session                                     = IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                UserAnswers.empty,
                None,
                None,
                taxChecks
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
              val session                                     = CompanyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                UserAnswers.empty,
                None,
                None,
                taxChecks
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
          val session                                     = IndividualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.ScrapMetalDealerSite)
            ),
            None,
            None,
            List.empty
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
                val session                                     = IndividualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  UserAnswers.empty.copy(
                    licenceType = Some(licenceType)
                  ),
                  None,
                  None,
                  List.empty
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
          val session                                     = IndividualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(EntityType.Company)
            ),
            None,
            None,
            List.empty
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.EntityTypeController.wrongGGAccount()
          )

          result shouldBe routes.EntityTypeController.entityType()
        }

        "the wrong entity type page" in {
          val session                                     = IndividualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
            ),
            None,
            None,
            List.empty
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

            val session                                     = IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              ),
              None,
              None,
              List.empty
            )
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.TaxSituationController.taxSituation()
            )

            result shouldBe routes.LicenceDetailsController.recentLicenceLength()
          }

          "the licence type is not 'driver of taxis' and the entity type is correct" in {
            val session                                     = IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
                entityType = Some(EntityType.Individual)
              ),
              None,
              None,
              List.empty
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
            CompleteUserAnswers(
              licenceType,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              Some(taxSituation),
              Some(YesNoAnswer.Yes),
              entityType,
              None,
              None,
              None
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
          val session                                     = CompanyHECSession(
            companyLoginData,
            CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test Tech Ltd"))),
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            ),
            None,
            None,
            List.empty
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CRNController.companyRegistrationNumber()
          )

          result shouldBe routes.EntityTypeController.entityType
        }

        "the confirm company details page" in {
          val session                                     = CompanyHECSession(
            companyLoginData,
            CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test Tech Ltd"))),
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            ),
            None,
            None,
            List.empty
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CompanyDetailsController.confirmCompanyDetails()
          )

          result shouldBe routes.CRNController.companyRegistrationNumber()

        }

        "the company details not found page" in {
          val session                                     = CompanyHECSession(
            companyLoginData,
            CompanyRetrievedJourneyData.empty,
            UserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles),
              entityType = Some(Company),
              crn = Some(CRN("123456"))
            ),
            None,
            None,
            List.empty
          )
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.CompanyDetailsNotFoundController.companyNotFound()
          )

          result shouldBe routes.CRNController.companyRegistrationNumber()

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
            IncompleteUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToOneYear),
              Some(taxSituation),
              Some(YesNoAnswer.Yes),
              None,
              None,
              None,
              None
            ),
            None,
            None,
            List.empty
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
            Some(YesNoAnswer.Yes),
            Some(EntityType.Individual),
            None,
            None,
            None
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

    "JourneyServiceImpl.allAnswersComplete" when {
      "session is individual" must {
        val incompleteAnswersBase = IncompleteUserAnswers(
          licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires),
          licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears),
          licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
          taxSituation = Some(TaxSituation.PAYE),
          saIncomeDeclared = None,
          entityType = None,
          None,
          None,
          None
        )

        "return false when licence type is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase.copy(licenceType = None),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "return false when licence time trading is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase.copy(licenceTimeTrading = None),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "return false when licence validity period is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase.copy(licenceValidityPeriod = None),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "return false when entity type is present" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase.copy(entityType = Some(EntityType.Individual)),
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe false
        }

        "return true when entity type is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase,
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe true
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
                IndividualHECSession.newSession(individualLoginData)
              ) shouldBe true

              // income declared is present
              JourneyServiceImpl.allAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = Some(YesNoAnswer.Yes)
                ),
                IndividualHECSession.newSession(individualLoginData)
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
              val journeyData =
                IndividualRetrievedJourneyData(
                  saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), saStatus))
                )

              val session =
                IndividualHECSession.newSession(individualLoginData).copy(retrievedJourneyData = journeyData)

              // income declared is missing
              JourneyServiceImpl.allAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = None
                ),
                session
              ) shouldBe true

              // income declared is present
              JourneyServiceImpl.allAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = Some(YesNoAnswer.Yes)
                ),
                session
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
              val journeyData = IndividualRetrievedJourneyData(
                saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )

              val session =
                IndividualHECSession.newSession(individualLoginData).copy(retrievedJourneyData = journeyData)

              JourneyServiceImpl.allAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = Some(YesNoAnswer.Yes)
                ),
                session
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
              val journeyData = IndividualRetrievedJourneyData(
                saStatus = Some(SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )

              val session =
                IndividualHECSession.newSession(individualLoginData).copy(retrievedJourneyData = journeyData)

              JourneyServiceImpl.allAnswersComplete(
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

      "session is company" must {
        val incompleteAnswersBase = IncompleteUserAnswers(
          licenceType = Some(LicenceType.ScrapMetalDealerSite),
          licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears),
          licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear),
          taxSituation = None,
          saIncomeDeclared = None,
          entityType = None,
          None,
          None,
          None
        )

        "return false when licence type is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase.copy(licenceType = None),
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "return false when licence time trading is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase.copy(licenceTimeTrading = None),
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "return false when licence validity period is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteAnswersBase.copy(licenceValidityPeriod = None),
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "entity type is missing" in {
          JourneyServiceImpl.allAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase,
            CompanyHECSession.newSession(companyLoginData)
          ) shouldBe false
        }

        "return true when chargeable for CT answer is No" in {
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
                  crn = Some(CRN("1234567")),
                  companyDetailsConfirmed = Some(YesNoAnswer.Yes),
                  chargeableForCT = Some(YesNoAnswer.No)
                ),
                CompanyHECSession.newSession(companyLoginData)
              ) shouldBe true
            }
          }
        }

        "return true when chargeable for CT answer is Yes and CT status = NoticeToFileIssued" in {
          val date = LocalDate.now()
          JourneyServiceImpl.allAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase.copy(
              entityType = Some(EntityType.Company),
              taxSituation = None,
              saIncomeDeclared = None,
              crn = Some(CRN("1234567")),
              companyDetailsConfirmed = Some(YesNoAnswer.Yes),
              chargeableForCT = Some(YesNoAnswer.Yes)
            ),
            CompanyHECSession(
              companyLoginData,
              CompanyRetrievedJourneyData(
                None,
                None,
                Some(
                  CTStatusResponse(
                    CTUTR("utr"),
                    date,
                    date,
                    Some(CTAccountingPeriod(date, date, CTStatus.NoticeToFileIssued))
                  )
                )
              ),
              UserAnswers.empty,
              None,
              None,
              List.empty
            )
          ) shouldBe true
        }

        def testForChargeableForCtIsYes(status: CTStatus) = {
          val date = LocalDate.now()
          JourneyServiceImpl.allAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase.copy(
              entityType = Some(EntityType.Company),
              taxSituation = None,
              saIncomeDeclared = None,
              crn = Some(CRN("1234567")),
              companyDetailsConfirmed = Some(YesNoAnswer.Yes),
              chargeableForCT = Some(YesNoAnswer.Yes)
            ),
            CompanyHECSession(
              companyLoginData,
              CompanyRetrievedJourneyData(
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
              ),
              UserAnswers.empty,
              None,
              None,
              List.empty
            )
          ) shouldBe false
        }

        "return true when chargeable for CT answer is Yes and CT status = ReturnFound" in {
          testForChargeableForCtIsYes(CTStatus.ReturnFound)
        }

        "return true when chargeable for CT answer is Yes and CT status = NoReturnFound" in {
          testForChargeableForCtIsYes(CTStatus.NoReturnFound)
        }
      }

    }

  }

}
