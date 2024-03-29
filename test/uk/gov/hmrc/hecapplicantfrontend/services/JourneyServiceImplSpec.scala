/*
 * Copyright 2023 HM Revenue & Customs
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

import cats.implicits.catsSyntaxOptionId
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.mvc.{Call, MessagesRequest}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.{AuthenticatedRequest, RequestWithSessionData}
import uk.gov.hmrc.hecapplicantfrontend.controllers.{ControllerSpec, SessionSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckExit
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.{Company, Individual}
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.TaxSituation.PAYE
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.emailSend.EmailSendResult
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{Passcode, PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.{EmailAddressAlreadyVerified, MaximumNumberOfEmailsExceeded, PasscodeSent}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTAccountingPeriod.CTAccountingPeriodDigital
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.{CTAccountingPeriod, CTStatus, CTStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.{SAStatus, SAStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod.UpToOneYear
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class JourneyServiceImplSpec extends ControllerSpec with SessionSupport with AuditServiceSupport {

  val maxTaxChecksPerLicenceType                    = 3
  override lazy val additionalConfig: Configuration = Configuration(
    ConfigFactory.parseString(s"max-tax-checks-per-licence-type = $maxTaxChecksPerLicenceType")
  )
  implicit val appConf: AppConfig                   = appConfig

  val journeyService: JourneyServiceImpl = new JourneyServiceImpl(mockSessionStore, mockAuditService)

  val taxCheckStartDateTime = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))
  val fakeRequest           = new MessagesRequest(FakeRequest(), messagesApi)

  def requestWithSessionData(s: HECSession, language: Language = Language.English): RequestWithSessionData[_] =
    RequestWithSessionData(AuthenticatedRequest(fakeRequest), s, language)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  val individualLoginData: IndividualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)

  val companyLoginData: CompanyLoginData =
    CompanyLoginData(GGCredId(""), None, None, None)

  val companyLoginData1: CompanyLoginData =
    CompanyLoginData(GGCredId(""), Some(CTUTR("4444444444")), None, None)

  val ggEmailId = EmailAddress("user@test.com")

  val otherEmailId = EmailAddress("user1@test.com")

  "JourneyServiceImpl" when {

    "handling calls to 'firstPage'" when {

      "tax checks list is not empty " must {

        val taxChecks = List(
          TaxCheckListItem(
            LicenceType.DriverOfTaxisAndPrivateHires,
            HECTaxCheckCode("some-code"),
            LocalDate.now(),
            ZonedDateTime.now()
          )
        )

        "return the correct call" when afterWord("the user is") {

          "an individual and hasConfirmedDetails = true" in {
            val session = IndividualHECSession
              .newSession(individualLoginData)
              .copy(unexpiredTaxChecks = taxChecks, hasConfirmedDetails = true)
            journeyService.firstPage(session) shouldBe routes.TaxChecksListController.unexpiredTaxChecks
          }

          "an individual and hasConfirmedDetails = false" in {
            val session = IndividualHECSession
              .newSession(individualLoginData)
              .copy(unexpiredTaxChecks = taxChecks)
            journeyService.firstPage(
              session
            ) shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails
          }

          "a company" in {
            val session = CompanyHECSession.newSession(companyLoginData).copy(unexpiredTaxChecks = taxChecks)
            journeyService.firstPage(session) shouldBe routes.TaxChecksListController.unexpiredTaxChecks
          }
        }
      }

      "tax checks list is empty" must {

        "return the correct call" when afterWord("the user is") {

          "an individual and hasConfirmedDetails = true" in {
            val session = IndividualHECSession.newSession(individualLoginData).copy(hasConfirmedDetails = true)
            journeyService.firstPage(session) shouldBe routes.LicenceDetailsController.licenceType
          }

          "an individual and hasConfirmedDetails = false" in {
            val session = IndividualHECSession.newSession(individualLoginData)
            journeyService.firstPage(
              session
            ) shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails
          }

          "a company" in {
            val session = CompanyHECSession.newSession(companyLoginData)
            journeyService.firstPage(session) shouldBe routes.LicenceDetailsController.licenceType
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
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit,
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
            routes.ConfirmIndividualDetailsController.confirmIndividualDetails,
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
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails,
              session
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceType)
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
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails,
              session
            )
            await(result.value) shouldBe Right(routes.TaxChecksListController.unexpiredTaxChecks)
          }
        }

        "the tax check codes page" when {

          def test(session: HECSession, expectedNext: Call) = {
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.TaxChecksListController.unexpiredTaxChecks,
              session
            )
            await(result.value) shouldBe Right(expectedNext)
          }

          "no email has been requested" in {
            test(
              IndividualHECSession.newSession(individualLoginData),
              routes.LicenceDetailsController.licenceType
            )
          }

          "an email has been requested but the origin url is no the tax check codes page" in {
            test(
              IndividualHECSession
                .newSession(individualLoginData)
                .copy(
                  emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck(originUrl = "/url").some
                ),
              routes.LicenceDetailsController.licenceType
            )
          }

          "an email has been requested and the origin url is the tax check codes page and" when {

            "there is a gg email in session" in {
              test(
                IndividualHECSession
                  .newSession(individualLoginData.copy(emailAddress = Some(EmailAddress("user@test.com"))))
                  .copy(
                    emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck(originUrl = "/url").some
                  ),
                routes.LicenceDetailsController.licenceType
              )
            }

            "there is no gg email in session" in {
              test(
                IndividualHECSession
                  .newSession(individualLoginData.copy(emailAddress = None))
                  .copy(
                    emailRequestedForTaxCheck = Fixtures
                      .emailRequestedForTaxCheck(originUrl = routes.TaxChecksListController.unexpiredTaxChecks.url)
                      .some
                  ),
                routes.EnterEmailAddressController.enterEmailAddress
              )
            }

          }

        }

        "the licence type page" when {

          "the user is an Individual" in {
            val taxiDriverTaxCheck = Fixtures.taxCheckListItem(LicenceType.DriverOfTaxisAndPrivateHires)

            val taxChecks = List.fill(maxTaxChecksPerLicenceType - 1)(taxiDriverTaxCheck)

            val session        = IndividualHECSession.newSession(individualLoginData).copy(unexpiredTaxChecks = taxChecks)
            val updatedSession =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)),
                unexpiredTaxChecks = taxChecks
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.licenceType,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceTimeTrading)
          }

          "the user is a Company" in {

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
              routes.LicenceDetailsController.licenceType,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.licenceTimeTrading)
          }

          "max tax check limit has already been exceeded" in {
            val taxiDriverLicenceType = LicenceType.DriverOfTaxisAndPrivateHires
            val taxiDriverTaxCheck    = Fixtures.taxCheckListItem(taxiDriverLicenceType)
            val otherTaxCheck         = Fixtures.taxCheckListItem(LicenceType.ScrapMetalDealerSite)

            val taxChecks = otherTaxCheck :: List.fill(maxTaxChecksPerLicenceType)(taxiDriverTaxCheck)

            val session        = IndividualHECSession
              .newSession(individualLoginData)
              .copy(unexpiredTaxChecks = taxChecks)
            val updatedSession =
              Fixtures.individualHECSession(
                loginData = individualLoginData,
                retrievedJourneyData = IndividualRetrievedJourneyData.empty,
                userAnswers = IndividualUserAnswers.empty.copy(licenceType = Some(taxiDriverLicenceType)),
                unexpiredTaxChecks = taxChecks
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session, Language.Welsh)

            inSequence {
              mockSendAuditEvent(TaxCheckExit.AllowedTaxChecksExceeded(updatedSession, Language.Welsh))
              mockStoreSession(updatedSession)(Right(()))
            }

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.licenceType,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.maxTaxChecksExceeded)
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
              routes.LicenceDetailsController.licenceTimeTrading,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.LicenceDetailsController.recentLicenceLength)
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
              routes.LicenceDetailsController.licenceTimeTrading,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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

            assertThrows[InconsistentSessionState] {
              journeyService.updateAndNext(
                routes.LicenceDetailsController.recentLicenceLength,
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
              routes.LicenceDetailsController.recentLicenceLength,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.TaxSituationController.taxSituation)
          }

          "the licence type in the session is not 'driver of taxis' and" when {

            def test(entityType: EntityType, didConfirmUncertainEntityType: Option[Boolean], expectedNext: Call): Unit =
              List(
                LicenceType.OperatorOfPrivateHireVehicles,
                LicenceType.ScrapMetalDealerSite,
                LicenceType.ScrapMetalMobileCollector
              ).foreach { licenceType =>
                withClue(s"For licence type $licenceType: ") {
                  val (session, updatedSession) =
                    entityType match {
                      case EntityType.Individual =>
                        val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(licenceType))
                        val loginData      =
                          individualLoginData.copy(didConfirmUncertainEntityType = didConfirmUncertainEntityType)
                        val session        =
                          Fixtures.individualHECSession(
                            loginData,
                            IndividualRetrievedJourneyData.empty,
                            answers
                          )
                        val updatedSession =
                          Fixtures.individualHECSession(
                            loginData,
                            IndividualRetrievedJourneyData.empty,
                            answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear))
                          )

                        (session, updatedSession)

                      case EntityType.Company =>
                        val answers        = CompanyUserAnswers.empty.copy(licenceType = Some(licenceType))
                        val loginData      =
                          companyLoginData.copy(didConfirmUncertainEntityType = didConfirmUncertainEntityType)
                        val session        =
                          Fixtures.companyHECSession(
                            loginData,
                            CompanyRetrievedJourneyData.empty,
                            answers
                          )
                        val updatedSession =
                          Fixtures.companyHECSession(
                            loginData,
                            CompanyRetrievedJourneyData.empty,
                            answers.copy(licenceValidityPeriod = Some(LicenceValidityPeriod.UpToOneYear))
                          )
                        (session, updatedSession)
                    }

                  implicit val request: RequestWithSessionData[_] =
                    requestWithSessionData(session)

                  mockStoreSession(updatedSession)(Right(()))

                  val result = journeyService.updateAndNext(
                    routes.LicenceDetailsController.recentLicenceLength,
                    updatedSession
                  )
                  await(result.value) shouldBe Right(expectedNext)
                }
              }

            "the user is an individual and" when {

              "didConfirmUncertainEntityType is true" in {
                test(EntityType.Individual, Some(true), routes.TaxSituationController.taxSituation)
              }

              "didConfirmUncertainEntityType is false" in {
                test(EntityType.Individual, Some(false), routes.EntityTypeController.entityType)
              }

              "didConfirmUncertainEntityType is not defined" in {
                test(EntityType.Individual, None, routes.EntityTypeController.entityType)
              }
            }

            "the user is a company and" when {

              "didConfirmUncertainEntityType is true" in {
                test(EntityType.Company, Some(true), routes.CRNController.companyRegistrationNumber)
              }

              "didConfirmUncertainEntityType is false" in {
                test(EntityType.Company, Some(false), routes.EntityTypeController.entityType)
              }

              "didConfirmUncertainEntityType is not defined" in {
                test(EntityType.Company, None, routes.EntityTypeController.entityType)
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
              routes.EntityTypeController.entityType,
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

            assertThrows[InconsistentSessionState] {
              journeyService.updateAndNext(
                routes.EntityTypeController.entityType,
                session
              )
            }
          }

          "the user is a individual but has selected company" in {
            test(individualLoginData, EntityType.Company, routes.EntityTypeController.wrongGGAccount)
          }

          "the user is a company but has selected individual" in {
            test(companyLoginData, EntityType.Individual, routes.EntityTypeController.wrongGGAccount)
          }

          "the user is a individual and  has selected individual" in {
            test(individualLoginData, EntityType.Individual, routes.TaxSituationController.taxSituation)
          }

          "the user is a company and  has selected company" in {
            test(companyLoginData, EntityType.Company, routes.CRNController.companyRegistrationNumber)
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
              None,
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

            assertThrows[InconsistentSessionState](
              journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation,
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
              routes.TaxSituationController.taxSituation,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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

              val result = journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation,
                session
              )

              await(result.value) shouldBe Right(routes.SAController.sautrNotFound)
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
                requestWithSessionData(session, Language.English)

              inSequence {
                mockSendAuditEvent(TaxCheckExit.SAUTRNotFound(updatedSession, Language.English))
                mockStoreSession(updatedSession)(Right(()))
              }

              val result = journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.SAController.sautrNotFound)
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

              assertThrows[InconsistentSessionState](
                journeyService.updateAndNext(
                  routes.TaxSituationController.taxSituation,
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
                routes.TaxSituationController.taxSituation,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.SAController.saIncomeStatement)
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
                requestWithSessionData(session, Language.Welsh)

              inSequence {
                mockSendAuditEvent(TaxCheckExit.SANoNoticeToFileOrTaxReturn(updatedSession, Language.Welsh))
                mockStoreSession(updatedSession)(Right(()))
              }
              val result = journeyService.updateAndNext(
                routes.TaxSituationController.taxSituation,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.SAController.noReturnFound)
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
                routes.TaxSituationController.taxSituation,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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
            routes.SAController.saIncomeStatement,
            session
          )
          await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
        }

        "the check your answers page" when {

          "all user answers are not complete" in {
            val session                                     = IndividualHECSession.newSession(individualLoginData)
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[InconsistentSessionState](
              await(
                journeyService
                  .updateAndNext(
                    routes.CheckYourAnswersController.checkYourAnswers,
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
              routes.CheckYourAnswersController.checkYourAnswers,
              session
            )
            await(result.value) shouldBe Right(routes.TaxCheckCompleteController.taxCheckComplete)

          }

        }

        "the company registration number page" when {

          "the CRN is blocked" in {
            val sessionWithBlockedCrn = Fixtures.companyHECSession(companyLoginData, crnBlocked = true)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(sessionWithBlockedCrn)

            val result = journeyService.updateAndNext(
              routes.CRNController.companyRegistrationNumber,
              sessionWithBlockedCrn
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.tooManyCtutrAttempts)
          }

          "the company is found" in {
            val session        = CompanyHECSession.newSession(companyLoginData)
            val updatedSession =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty.copy(companyName = Some(CompanyHouseName("Test tech Ltd"))),
                CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567")))
              )

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(updatedSession)(Right(()))

            val result = journeyService.updateAndNext(
              routes.CRNController.companyRegistrationNumber,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.confirmCompanyDetails)
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

            assertThrows[InconsistentSessionState](
              await(
                journeyService
                  .updateAndNext(
                    routes.CRNController.companyRegistrationNumber,
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
              routes.CompanyDetailsController.confirmCompanyDetails,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CRNController.companyRegistrationNumber)
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

            assertThrows[InconsistentSessionState] {
              journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails,
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

            assertThrows[InconsistentSessionState] {
              journeyService.updateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails,
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
                CTAccountingPeriodDigital(
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
              routes.CompanyDetailsController.confirmCompanyDetails,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.ctutrNotMatched)
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
              routes.CompanyDetailsController.confirmCompanyDetails,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck)
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
                routes.CompanyDetailsController.confirmCompanyDetails,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.chargeableForCorporationTax)
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
                routes.CompanyDetailsController.confirmCompanyDetails,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.recentlyStartedTrading)
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
                routes.CompanyDetailsController.confirmCompanyDetails,
                updatedSession
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck)
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
              routes.CompanyDetailsController.confirmCompanyDetails,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.enterCtutr)
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
              routes.CompanyDetailsController.confirmCompanyDetails,
              updatedSession
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck)
          }

        }

        "chargeable for CT page" should {
          val chargeableForCorporationTaxRoute = routes.CompanyDetailsController.chargeableForCorporationTax
          val date                             = LocalDate.now

          "throw" when {
            "the applicant is an individual" in {
              val session        = IndividualHECSession.newSession(individualLoginData)
              val updatedSession = session.copy(userAnswers = IndividualUserAnswers.empty)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[InconsistentSessionState] {
                journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              }
            }

            "applicant is company but chargeable for CT answer is missing" in {
              val session        = CompanyHECSession.newSession(companyLoginData)
              val updatedSession = session.copy(userAnswers = CompanyUserAnswers.empty.copy(crn = Some(CRN("1234567"))))

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[InconsistentSessionState] {
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

              assertThrows[InconsistentSessionState] {
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
                  ctStatus = Some(
                    CTStatusResponse(CTUTR("utr"), date, date, Some(CTAccountingPeriodDigital(date, date, status)))
                  )
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
                await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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
                  Some(CTAccountingPeriodDigital(date, date, status))
                )
              )
            )

            val yesUserAnswers = CompanyUserAnswers.empty.copy(
              crn = Some(CRN("1234567")),
              chargeableForCT = Some(YesNoAnswer.Yes)
            )

            def test(status: CTStatus, destination: Call, auditEvent: Option[HECSession => AuditEvent]) = {
              val session        =
                Fixtures.companyHECSession(companyLoginData, companyData(status), CompanyUserAnswers.empty)
              val updatedSession = session.copy(userAnswers = yesUserAnswers)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              inSequence {
                auditEvent.foreach(event => mockSendAuditEvent(event(updatedSession)))
                mockStoreSession(updatedSession)(Right(()))
              }

              val result = journeyService.updateAndNext(chargeableForCorporationTaxRoute, updatedSession)
              await(result.value) shouldBe Right(destination)
            }

            "status = NoticeToFileIssued" in {
              test(
                status = CTStatus.NoticeToFileIssued,
                destination = routes.CheckYourAnswersController.checkYourAnswers,
                None
              )
            }

            "status = ReturnFound" in {
              test(
                status = CTStatus.ReturnFound,
                destination = routes.CompanyDetailsController.ctIncomeStatement,
                None
              )
            }

            "status = NoReturnFound" in {
              test(
                status = CTStatus.NoReturnFound,
                destination = routes.CompanyDetailsController.cannotDoTaxCheck,
                Some(TaxCheckExit.CTNoNoticeToFileOrTaxReturn(_, Language.English))
              )
            }
          }

        }

        "CT income statement page" in {
          val session                                     = CompanyHECSession.newSession(companyLoginData)
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.updateAndNext(
            routes.CompanyDetailsController.ctIncomeStatement,
            session
          )
          await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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

            assertThrows[InconsistentSessionState] {
              journeyService.updateAndNext(
                routes.CompanyDetailsController.recentlyStartedTrading,
                updatedSession
              )
            }
          }

          def testStartTrading(answer: YesNoAnswer, nextCall: Call, auditEvent: Option[HECSession => AuditEvent]) = {
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

            inSequence {
              auditEvent.foreach(event => mockSendAuditEvent(event(updatedSession)))
              mockStoreSession(updatedSession)(Right(()))
            }

            val result =
              journeyService.updateAndNext(routes.CompanyDetailsController.recentlyStartedTrading, updatedSession)
            await(result.value) shouldBe Right(nextCall)
          }

          "applicant select yes" in {
            testStartTrading(YesNoAnswer.Yes, routes.CheckYourAnswersController.checkYourAnswers, None)
          }

          "applicant select no" in {
            testStartTrading(
              YesNoAnswer.No,
              routes.CompanyDetailsController.cannotDoTaxCheck,
              Some(TaxCheckExit.CTNoAccountingPeriodNotRecentlyStartedTrading(_, Language.English))
            )
          }

        }

        "enter CTUTR page" when {

          "number of attempts has reached the maximum & no valid CTUTR was found" in {
            val session = Fixtures.companyHECSession(crnBlocked = true)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.CompanyDetailsController.enterCtutr,
              session
            )
            await(result.value) shouldBe Right(routes.CompanyDetailsController.tooManyCtutrAttempts)
          }

          "number of attempts has not reached the maximum" must {

            "throw error if CTUTR is missing" in {
              val session = Fixtures.companyHECSession()

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              assertThrows[InconsistentSessionState] {
                journeyService.updateAndNext(
                  routes.CompanyDetailsController.enterCtutr,
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

              assertThrows[InconsistentSessionState] {
                journeyService.updateAndNext(
                  routes.CompanyDetailsController.enterCtutr,
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

              assertThrows[InconsistentSessionState] {
                journeyService.updateAndNext(
                  routes.CompanyDetailsController.enterCtutr,
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
                routes.CompanyDetailsController.enterCtutr,
                session
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.cannotDoTaxCheck)
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
                routes.CompanyDetailsController.enterCtutr,
                session
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.recentlyStartedTrading)
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
                routes.CompanyDetailsController.enterCtutr,
                session
              )
              await(result.value) shouldBe Right(routes.CompanyDetailsController.chargeableForCorporationTax)
            }
          }
        }

        "tax check complete page" when {

          def testIndividual(emailAddress: Option[EmailAddress], nextCall: Call) = {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = emailAddress),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now())),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck())
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.TaxCheckCompleteController.taxCheckComplete,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }

          def testCompany(emailAddress: Option[EmailAddress], nextCall: Call) = {

            val session                                     = Fixtures.companyHECSession(
              companyLoginData.copy(emailAddress = emailAddress),
              CompanyRetrievedJourneyData.empty,
              Fixtures.completeCompanyUserAnswers(recentlyStartedTrading = YesNoAnswer.Yes.some),
              taxCheckStartDateTime = Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck())
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.TaxCheckCompleteController.taxCheckComplete,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }

          "tax check code is generated for an Individual" when {
            "User has email id in GG account" in {
              testIndividual(
                Some(EmailAddress("user@test.com")),
                routes.ConfirmEmailAddressController.confirmEmailAddress
              )
            }

            "User don't has email id in GG account" in {
              testIndividual(None, routes.EnterEmailAddressController.enterEmailAddress)
            }

            "User  has email id in GG account but is invalid" in {
              testIndividual(
                Some(EmailAddress("user@123@test.com")),
                routes.EnterEmailAddressController.enterEmailAddress
              )
            }
          }

          "tax check code is generated for a company" when {
            "company has email id in GG account" in {
              testCompany(
                Some(EmailAddress("user@test.com")),
                routes.ConfirmEmailAddressController.confirmEmailAddress
              )
            }

            "company don't has email id in GG account" in {
              testCompany(None, routes.EnterEmailAddressController.enterEmailAddress)
            }

            "company has email id in GG account but is invalid" in {
              testCompany(
                Some(EmailAddress("user@123@test.com")),
                routes.EnterEmailAddressController.enterEmailAddress
              )
            }
          }

        }

        "the confirm email address page" when {

          def test(userEmailAnswers: UserEmailAnswers, nextCall: Call) = {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = ggEmailId.some),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck()),
              userEmailAnswers = userEmailAnswers.some
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.ConfirmEmailAddressController.confirmEmailAddress,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }

          "the valid ggEmailId is selected or valid different email id is entered and " when {

            " Email Verification Service response = Passcode Sent" in {
              List(
                Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, PasscodeSent.some),
                Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, PasscodeSent.some)
              ).foreach { eachUserEmailAddress =>
                withClue(s"For user email address : $eachUserEmailAddress") {
                  test(
                    eachUserEmailAddress,
                    routes.VerifyEmailPasscodeController.verifyEmailPasscode
                  )
                }
              }

            }

            " Email Verification Service response = Email Already Verified " in {

              List(
                Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, EmailAddressAlreadyVerified.some),
                Fixtures.userEmailAnswers(
                  EmailType.DifferentEmail,
                  otherEmailId,
                  EmailAddressAlreadyVerified.some
                )
              ).foreach { eachUserEmailAddress =>
                withClue(s"For user email address : $eachUserEmailAddress") {
                  test(
                    eachUserEmailAddress,
                    routes.EmailAddressConfirmedController.emailAddressConfirmed
                  )
                }
              }
            }

            " Email Verification Service response = Too Many Email attempts in session " in {

              List(
                Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, MaximumNumberOfEmailsExceeded.some),
                Fixtures.userEmailAnswers(
                  EmailType.DifferentEmail,
                  otherEmailId,
                  MaximumNumberOfEmailsExceeded.some
                )
              ).foreach { eachUserEmailAddress =>
                withClue(s"For user email address : $eachUserEmailAddress") {
                  test(
                    eachUserEmailAddress,
                    routes.TooManyEmailVerificationAttemptController.tooManyEmailVerificationAttempts
                  )
                }
              }
            }
          }
        }

        "the confirm email passcode page" when {

          def test(passcodeVerificationResult: PasscodeVerificationResult, nextCall: Call) = {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = ggEmailId.some),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck()),
              userEmailAnswers = Fixtures
                .userEmailAnswers(
                  passcodeRequestResult = PasscodeSent.some,
                  passcode = Passcode("HHHHHH").some,
                  passcodeVerificationResult = passcodeVerificationResult.some
                )
                .some
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.VerifyEmailPasscodeController.verifyEmailPasscode,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }

          "passcode is a match and verified" in {
            test(PasscodeVerificationResult.Match, routes.EmailAddressConfirmedController.emailAddressConfirmed)
          }

          "passcode is expired" in {
            test(
              PasscodeVerificationResult.Expired,
              routes.VerificationPasscodeExpiredController.verificationPasscodeExpired
            )
          }

          "passcode has been attempted too many times" in {
            test(
              PasscodeVerificationResult.TooManyAttempts,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification
            )
          }

        }

        "email address confirmed page" when {

          def test(emailSendResult: Option[EmailSendResult], nextCall: Call) = {
            val session                                     = Fixtures.companyHECSession(
              loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
              userAnswers = Fixtures.completeCompanyUserAnswers(),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck()),
              userEmailAnswers = Fixtures
                .userEmailAnswers(
                  passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                  passcode = Passcode("HHHHHH").some,
                  passcodeVerificationResult = PasscodeVerificationResult.Match.some,
                  emailSendResult = emailSendResult
                )
                .some
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.EmailAddressConfirmedController.emailAddressConfirmed,
              session
            )
            await(result.value) shouldBe Right(nextCall)

          }

          "the email is send" in {
            test(EmailSendResult.EmailSent.some, routes.EmailSentController.emailSent)
          }

          "the email is not send due to any failures" in {
            test(EmailSendResult.EmailSentFailure.some, routes.ProblemSendingEmailController.problemSendingEmail)
          }

        }

        "the enter email address page" when {
          def test(userEmailAnswers: UserEmailAnswers, nextCall: Call) = {
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
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck()),
              userEmailAnswers = userEmailAnswers.some
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.ConfirmEmailAddressController.confirmEmailAddress,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }
          "valid email id is entered and Email Verification Service response = Passcode Sent" in {
            test(
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, PasscodeSent.some),
              routes.VerifyEmailPasscodeController.verifyEmailPasscode
            )
          }

          "valid email id is entered and Email Verification Service response = Email Already Verified" in {
            test(
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, EmailAddressAlreadyVerified.some),
              routes.EmailAddressConfirmedController.emailAddressConfirmed
            )
          }

          "valid email id is entered and Email Verification Service response = Too Many Email attempts in session " in {
            test(
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, MaximumNumberOfEmailsExceeded.some),
              routes.TooManyEmailVerificationAttemptController.tooManyEmailVerificationAttempts
            )
          }

        }

        "resend email confirmation page" when {

          def test(userEmailAnswers: UserEmailAnswers, nextCall: Call) = {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = ggEmailId.some),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck()),
              hasResentEmailConfirmation = true,
              userEmailAnswers = userEmailAnswers.some
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.ResendEmailConfirmationController.resendEmail,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }

          " Email Verification Service response = Passcode Sent" in {
            List(
              Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, PasscodeSent.some),
              Fixtures.userEmailAnswers(EmailType.DifferentEmail, otherEmailId, PasscodeSent.some)
            ).foreach { eachUserEmailAddress =>
              withClue(s"For user email address : $eachUserEmailAddress") {
                test(
                  eachUserEmailAddress,
                  routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode
                )
              }
            }

          }

          " Email Verification Service response = Email Already Verified " in {

            List(
              Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, EmailAddressAlreadyVerified.some),
              Fixtures.userEmailAnswers(
                EmailType.DifferentEmail,
                otherEmailId,
                EmailAddressAlreadyVerified.some
              )
            ).foreach { eachUserEmailAddress =>
              withClue(s"For user email address : $eachUserEmailAddress") {
                test(
                  eachUserEmailAddress,
                  routes.EmailAddressConfirmedController.emailAddressConfirmed
                )
              }
            }
          }

          " Email Verification Service response = Too Many Email attempts in session " in {

            List(
              Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, MaximumNumberOfEmailsExceeded.some),
              Fixtures.userEmailAnswers(
                EmailType.DifferentEmail,
                otherEmailId,
                MaximumNumberOfEmailsExceeded.some
              )
            ).foreach { eachUserEmailAddress =>
              withClue(s"For user email address : $eachUserEmailAddress") {
                test(
                  eachUserEmailAddress,
                  routes.TooManyEmailVerificationAttemptController.tooManyEmailVerificationAttempts
                )
              }
            }
          }

        }

        "verify resend email confirmation code page" when {
          def test(passcodeVerificationResult: PasscodeVerificationResult, nextCall: Call) = {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = ggEmailId.some),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck()),
              hasResentEmailConfirmation = true,
              userEmailAnswers = Fixtures
                .userEmailAnswers(
                  passcodeRequestResult = PasscodeSent.some,
                  passcode = Passcode("HHHHHH").some,
                  passcodeVerificationResult = passcodeVerificationResult.some
                )
                .some
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.updateAndNext(
              routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode,
              session
            )
            await(result.value) shouldBe Right(nextCall)
          }

          "passcode is a match and verified" in {
            test(PasscodeVerificationResult.Match, routes.EmailAddressConfirmedController.emailAddressConfirmed)
          }

          "passcode is expired" in {
            test(
              PasscodeVerificationResult.Expired,
              routes.VerificationPasscodeExpiredController.verificationPasscodeExpired
            )
          }

          "passcode has been attempted too many times" in {
            test(
              PasscodeVerificationResult.TooManyAttempts,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification
            )
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
                saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoticeToFileIssued))
              )

            val session = Fixtures.individualHECSession(individualData, journeyData, incompleteAnswers)

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.licenceTimeTrading,
              session
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)

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
                  routes.LicenceDetailsController.licenceTimeTrading,
                  session
                )
                await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)

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
                  routes.LicenceDetailsController.licenceTimeTrading,
                  session
                )
                await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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
                  saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
                )
                val session     =
                  Fixtures.individualHECSession(
                    loginData = individualLoginData.copy(sautr = Some(SAUTR("utr"))),
                    retrievedJourneyData = journeyData,
                    userAnswers = incompleteAnswers
                  )

                implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
                mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

                val result = journeyService.updateAndNext(
                  routes.LicenceDetailsController.licenceTimeTrading,
                  session
                )
                await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
              }
            }
          }

          "the user has selected an SA tax situation, SA status = NoticeToFileIssued" in {
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
                  saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoticeToFileIssued))
                )
                val session     =
                  Fixtures.individualHECSession(
                    individualLoginData.copy(sautr = Some(SAUTR("utr"))),
                    journeyData,
                    incompleteAnswers
                  )

                implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)
                mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

                val result = journeyService.updateAndNext(
                  routes.LicenceDetailsController.licenceTimeTrading,
                  session
                )
                await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)
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
            ctStatusResponse: Option[CTStatusResponse],
            entityType: Option[EntityType],
            didConfirmUncertainEntityType: Option[Boolean]
          ): (CompleteCompanyUserAnswers, CompanyHECSession) = {

            val completeAnswers   = Fixtures.completeCompanyUserAnswers(
              licenceType,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              entityType = entityType,
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
              entityType = entityType,
              crn = Some(CRN("1123456")),
              companyDetailsConfirmed = Some(YesNoAnswer.Yes),
              chargeableForCT = chargeableForCTOpt,
              ctIncomeDeclared = ctIncomeDeclaredOpt,
              recentlyStartedTrading = recentlyStartedTradingOpt
            )

            val session = Fixtures.companyHECSession(
              Fixtures.companyLoginData(
                ctutr = Some(CTUTR("1111111111")),
                didConfirmUncertainEntityType = didConfirmUncertainEntityType
              ),
              Fixtures.companyRetrievedJourneyData(ctStatus = ctStatusResponse),
              userAnswers = incompleteAnswers
            )
            (completeAnswers, session)
          }

          def nextPageIsCYA(session: CompanyHECSession, completeAnswers: CompanyUserAnswers) = {
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            mockStoreSession(session.copy(userAnswers = completeAnswers))(Right(()))

            val result = journeyService.updateAndNext(
              routes.LicenceDetailsController.licenceTimeTrading,
              session
            )
            await(result.value) shouldBe Right(routes.CheckYourAnswersController.checkYourAnswers)

          }

          "the user has selected a licence type which both individuals and companies can have" in {
            List(
              LicenceType.ScrapMetalDealerSite,
              LicenceType.ScrapMetalMobileCollector,
              LicenceType.OperatorOfPrivateHireVehicles
            ).foreach { licenceType =>
              List(
                Some(EntityType.Company) -> Some(false),
                Some(EntityType.Company) -> None,
                None                     -> Some(true)
              ).foreach { case (entityType, didConfirmUncertainEntityType) =>
                withClue(
                  s"For licence type $licenceType, entity type answer $entityType, didConfirmUncertainEntityType = $didConfirmUncertainEntityType: "
                ) {

                  val (completeAnswers, session) = getCompanySessionData(
                    licenceType,
                    Some(YesNoAnswer.Yes),
                    Some(YesNoAnswer.Yes),
                    Some(YesNoAnswer.Yes),
                    Some(createCTStatus(Some(CTAccountingPeriodDigital(startDate, endDate, CTStatus.ReturnFound)))),
                    entityType,
                    didConfirmUncertainEntityType
                  )
                  nextPageIsCYA(session, completeAnswers)

                }
              }

            }

          }

          "the user has some ctStatus Response with latest accounting period status None, recentlyStartedTrading has Yes value " in {
            val (completeAnswers, session) = getCompanySessionData(
              LicenceType.ScrapMetalMobileCollector,
              recentlyStartedTradingOpt = Some(YesNoAnswer.Yes),
              ctStatusResponse = Some(createCTStatus(None)),
              ctIncomeDeclaredOpt = None,
              chargeableForCTOpt = None,
              entityType = Some(EntityType.Company),
              didConfirmUncertainEntityType = Some(false)
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
                ctStatusResponse = Some(
                  createCTStatus(Some(CTAccountingPeriodDigital(startDate, endDate, CTStatus.NoticeToFileIssued)))
                ),
                ctIncomeDeclaredOpt = None,
                chargeableForCTOpt = Some(yesNo),
                entityType = Some(EntityType.Company),
                didConfirmUncertainEntityType = None
              )
              nextPageIsCYA(session, completeAnswers)
            }
          }

          "the user has some ctStatus Response with latest accounting period status NoReturnFound, chargeableForCT has No value " in {
            val (completeAnswers, session) = getCompanySessionData(
              LicenceType.ScrapMetalMobileCollector,
              recentlyStartedTradingOpt = None,
              ctStatusResponse =
                Some(createCTStatus(Some(CTAccountingPeriodDigital(startDate, endDate, CTStatus.NoReturnFound)))),
              ctIncomeDeclaredOpt = None,
              chargeableForCTOpt = Some(YesNoAnswer.No),
              entityType = None,
              didConfirmUncertainEntityType = Some(true)
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
            routes.EntityTypeController.entityType,
            session
          )
          await(result.value) shouldBe Right(routes.EntityTypeController.wrongGGAccount)
        }

      }

    }

    "handling calls to 'previous'" must {

      "return an error" when {

        "no previous location can be found" in {
          val session                                     = CompanyHECSession.newSession(companyLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)
          assertThrows[InconsistentSessionState](
            journeyService.previous(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetailsSubmit
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
            routes.StartController.start
          )

          result shouldBe routes.StartController.start
        }

        "the confirm individual details page" when {

          def test(session: HECSession, expectedPrevious: Call) = {
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.ConfirmIndividualDetailsController.confirmIndividualDetails
            )

            result shouldBe expectedPrevious
          }

          "the user did not have to confirm their entity type" in {
            test(
              IndividualHECSession.newSession(individualLoginData.copy(didConfirmUncertainEntityType = Some(false))),
              routes.StartController.start
            )
          }

          "the user did confirm their entity type" in {
            test(
              IndividualHECSession.newSession(individualLoginData.copy(didConfirmUncertainEntityType = Some(true))),
              routes.ConfirmUncertainEntityTypeController.entityType
            )
          }
        }

        "the confirm individual details exit page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.ConfirmIndividualDetailsController.confirmIndividualDetailsExit
          )

          result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails
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

            val result = journeyService.previous(routes.TaxChecksListController.unexpiredTaxChecks)
            result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails
          }

          "applicant is company and" when {

            "the user did not have to confirm their entity type" in {
              val session = CompanyHECSession.newSession(companyLoginData).copy(unexpiredTaxChecks = taxChecks)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              val result = journeyService.previous(routes.TaxChecksListController.unexpiredTaxChecks)
              result shouldBe routes.StartController.start
            }

            "the user did have to confirm their entity type" in {
              val session = CompanyHECSession
                .newSession(companyLoginData.copy(didConfirmUncertainEntityType = Some(true)))
                .copy(unexpiredTaxChecks = taxChecks)

              implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

              val result = journeyService.previous(routes.TaxChecksListController.unexpiredTaxChecks)
              result shouldBe routes.ConfirmUncertainEntityTypeController.entityType

            }

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
                routes.LicenceDetailsController.licenceType
              )

              result shouldBe routes.TaxChecksListController.unexpiredTaxChecks
            }

            "there are no preexisting tax check codes" in {
              val session                                     = IndividualHECSession.newSession(individualLoginData)
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType
              )

              result shouldBe routes.ConfirmIndividualDetailsController.confirmIndividualDetails
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
                routes.LicenceDetailsController.licenceType
              )

              result shouldBe routes.TaxChecksListController.unexpiredTaxChecks
            }

            "there are no preexisting tax check codes" in {
              val session                                     = CompanyHECSession.newSession(companyLoginData)
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType
              )

              result shouldBe routes.StartController.start
            }

            "the user had to confirm their entity type and there are no preexisting tax check codes" in {
              val session                                     =
                CompanyHECSession.newSession(companyLoginData.copy(didConfirmUncertainEntityType = Some(true)))
              implicit val request: RequestWithSessionData[_] =
                requestWithSessionData(session)

              val result = journeyService.previous(
                routes.LicenceDetailsController.licenceType
              )

              result shouldBe routes.ConfirmUncertainEntityTypeController.entityType
            }
          }

        }

        "maximum tax checks limit exceeded page" in {
          val taxiDriverLicenceType = LicenceType.DriverOfTaxisAndPrivateHires
          val taxiDriverTaxCheck    = Fixtures.taxCheckListItem(taxiDriverLicenceType)
          val otherTaxCheck         = Fixtures.taxCheckListItem(LicenceType.ScrapMetalDealerSite)

          val taxChecks = List(
            taxiDriverTaxCheck,
            taxiDriverTaxCheck,
            taxiDriverTaxCheck,
            taxiDriverTaxCheck,
            otherTaxCheck
          )

          val session = Fixtures.individualHECSession(
            loginData = individualLoginData,
            retrievedJourneyData = IndividualRetrievedJourneyData.empty,
            userAnswers =
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)),
            unexpiredTaxChecks = taxChecks
          )

          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.maxTaxChecksExceeded
          )

          result shouldBe routes.LicenceDetailsController.licenceType
        }

        "the licence type exit page" in {
          val session                                     = IndividualHECSession.newSession(individualLoginData)
          implicit val request: RequestWithSessionData[_] =
            requestWithSessionData(session)

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceTypeExit
          )

          result shouldBe routes.LicenceDetailsController.licenceType
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
            routes.LicenceDetailsController.licenceTimeTrading
          )

          result shouldBe routes.LicenceDetailsController.licenceType
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
                  routes.EntityTypeController.entityType
                )

                result shouldBe routes.LicenceDetailsController.recentLicenceLength
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
            routes.EntityTypeController.wrongGGAccount
          )

          result shouldBe routes.EntityTypeController.entityType
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
            routes.EntityTypeController.wrongEntityType
          )

          result shouldBe routes.EntityTypeController.entityType
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
              routes.TaxSituationController.taxSituation
            )

            result shouldBe routes.LicenceDetailsController.recentLicenceLength
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
              routes.TaxSituationController.taxSituation
            )

            result shouldBe routes.EntityTypeController.entityType
          }

        }

        "the check your answers page" when {

          def testPrevPageIsTaxSituation(session: HECSession) = {
            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            val result = journeyService.previous(
              routes.CheckYourAnswersController.checkYourAnswers
            )

            result shouldBe routes.TaxSituationController.taxSituation
          }

          val individualWithSautr =
            individualLoginData.copy(sautr = Some(SAUTR("utr")))

          val journeyDataWithSaStatus = IndividualRetrievedJourneyData(saStatus =
            Some(individual.SAStatusResponse(SAUTR(""), TaxYear(2020), SAStatus.NoticeToFileIssued))
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
            val session = Fixtures.individualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(LicenceType.DriverOfTaxisAndPrivateHires, TaxSituation.SA, None)
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = SAPAYE & SA status = NoticeToFileIssued" in {
            val session = Fixtures.individualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(LicenceType.DriverOfTaxisAndPrivateHires, TaxSituation.SAPAYE, None)
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = PAYE" in {
            val session = Fixtures.individualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                TaxSituation.PAYE,
                None
              )
            )

            testPrevPageIsTaxSituation(session)
          }

          "tax situation = Not Chargeable" in {
            val session = Fixtures.individualHECSession(
              individualWithSautr,
              journeyDataWithSaStatus,
              userAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                TaxSituation.NotChargeable,
                None
              )
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
              routes.SAController.saIncomeStatement
            )

            result shouldBe routes.TaxSituationController.taxSituation
          }

          def testThrows(taxSituation: TaxSituation) = {
            val session = buildIndividualSession(taxSituation, SAStatus.ReturnFound)

            implicit val request: RequestWithSessionData[_] =
              requestWithSessionData(session)

            assertThrows[InconsistentSessionState] {
              journeyService.previous(
                routes.SAController.saIncomeStatement
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
              routes.SAController.noReturnFound
            )

            result shouldBe routes.TaxSituationController.taxSituation
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

            assertThrows[InconsistentSessionState] {
              journeyService.previous(
                routes.SAController.noReturnFound
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
            routes.CRNController.companyRegistrationNumber
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
            routes.CompanyDetailsController.confirmCompanyDetails
          )

          result shouldBe routes.CRNController.companyRegistrationNumber

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
            routes.CompanyDetailsController.recentlyStartedTrading
          )

          result shouldBe routes.CompanyDetailsController.confirmCompanyDetails
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
            routes.CompanyDetailsController.enterCtutr
          )

          result shouldBe routes.CompanyDetailsController.confirmCompanyDetails

        }

        "the 'dont have' CTUTR' page" in {
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(Fixtures.companyHECSession())
          val result                                      = journeyService.previous(
            routes.CompanyDetailsController.dontHaveUtr
          )

          result shouldBe routes.CompanyDetailsController.enterCtutr
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
            routes.CompanyDetailsController.tooManyCtutrAttempts
          )

          result shouldBe routes.CRNController.companyRegistrationNumber
        }

        "the Confirm Email Address page" when {

          val emailJourneyOriginUrl = "/url"

          def test(userEmailAnswers: Option[UserEmailAnswers], previousRoute: Call) = {
            val journeyDataWithSaStatus                     = IndividualRetrievedJourneyData(saStatus =
              Some(individual.SAStatusResponse(SAUTR(""), TaxYear(2020), SAStatus.NoticeToFileIssued))
            )
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = EmailAddress("user@test.com").some),
              retrievedJourneyData = journeyDataWithSaStatus,
              Fixtures.completeIndividualUserAnswers(),
              Some(HECTaxCheck(HECTaxCheckCode("code1"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              taxCheckStartDateTime = taxCheckStartDateTime.some,
              emailRequestedForTaxCheck =
                Fixtures.emailRequestedForTaxCheck().copy(originUrl = emailJourneyOriginUrl).some,
              userEmailAnswers = userEmailAnswers
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(
              routes.ConfirmEmailAddressController.confirmEmailAddress
            )
            result shouldBe previousRoute
          }

          "the page is reached via too many passcode attempts page" in {

            test(
              userEmailAnswers = Fixtures
                .userEmailAnswers(passcodeVerificationResult = PasscodeVerificationResult.TooManyAttempts.some)
                .some,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification
            )

          }

          "the page is not reached via too many passcode attempts page" in {
            test(userEmailAnswers = None, Call("GET", emailJourneyOriginUrl))

          }
        }

        def previousIsEmailAddressConfirmed(emailSendResult: EmailSendResult, presentRoute: Call) = {
          val hecTaxCheckCode                             = HECTaxCheckCode("ABC 123 DER")
          val expiryDate                                  = LocalDate.of(2021, 10, 9)
          val hecTaxCheck                                 = HECTaxCheck(hecTaxCheckCode, expiryDate, ZonedDateTime.now)
          val userEmailAnswer                             = Fixtures
            .userEmailAnswers(
              passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
              passcode = Passcode("HHHHHH").some,
              passcodeVerificationResult = PasscodeVerificationResult.Match.some,
              emailSendResult = emailSendResult.some
            )
          val session                                     = Fixtures.companyHECSession(
            loginData = Fixtures.companyLoginData(emailAddress = ggEmailId.some),
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            completedTaxCheck = hecTaxCheck.some,
            userEmailAnswers = userEmailAnswer.some
          )
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.previous(presentRoute)
          result shouldBe routes.EmailAddressConfirmedController.emailAddressConfirmed
        }

        "the Email sent page" in {
          previousIsEmailAddressConfirmed(EmailSendResult.EmailSent, routes.EmailSentController.emailSent)
        }

        "the Email sent Failure page" in {
          previousIsEmailAddressConfirmed(
            EmailSendResult.EmailSentFailure,
            routes.ProblemSendingEmailController.problemSendingEmail
          )
        }

        "the Enter Email Address page" when {

          val emailJourneyOriginUrl = "/url"

          def test(
            emailAddress: Option[EmailAddress],
            userEmailAnswers: Option[UserEmailAnswers],
            previousRoute: Call,
            isResendFlag: Boolean
          ) = {
            val journeyDataWithSaStatus                     = IndividualRetrievedJourneyData(saStatus =
              Some(individual.SAStatusResponse(SAUTR(""), TaxYear(2020), SAStatus.NoticeToFileIssued))
            )
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = emailAddress),
              retrievedJourneyData = journeyDataWithSaStatus,
              Fixtures.completeIndividualUserAnswers(),
              Some(HECTaxCheck(HECTaxCheckCode("code1"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              taxCheckStartDateTime = taxCheckStartDateTime.some,
              emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck(originUrl = emailJourneyOriginUrl).some,
              userEmailAnswers = userEmailAnswers,
              hasResentEmailConfirmation = isResendFlag
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(
              routes.EnterEmailAddressController.enterEmailAddress
            )
            result shouldBe previousRoute

          }

          "the page is not reached via too many passcode attempts page" when {

            "the email id is invalid " in {
              test(
                Some(EmailAddress("user@test@test.com")),
                None,
                Call("GET", emailJourneyOriginUrl),
                false
              )
            }

            "the email id is not in GG login data" in {
              test(None, None, Call("GET", emailJourneyOriginUrl), false)
            }
          }

          "the page is reached via too many passcode attempts page" in {
            test(
              None,
              Fixtures
                .userEmailAnswers(
                  passcodeVerificationResult = PasscodeVerificationResult.TooManyAttempts.some,
                  passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                  passcode = Passcode("HHHHHH").some
                )
                .some,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification,
              true
            )
          }

        }

        "resend email confirmation page" when {

          def test(
            hasResendFlag: Boolean,
            passcodeVerificationResult: Option[PasscodeVerificationResult],
            previousRoute: Call
          ) = {
            val session                                     = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = ggEmailId.some),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
              userEmailAnswers = Fixtures
                .userEmailAnswers(
                  passcodeRequestResult = PasscodeRequestResult.PasscodeSent.some,
                  passcode = Passcode("HHHHHH").some,
                  passcodeVerificationResult = passcodeVerificationResult
                )
                .some,
              hasResentEmailConfirmation = hasResendFlag
            )
            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(
              routes.ResendEmailConfirmationController.resendEmail
            )
            result shouldBe previousRoute
          }

          "the page is reached via verify passcode page" in {
            test(false, None, routes.VerifyEmailPasscodeController.verifyEmailPasscode)
          }

          "the page is reached via verify resend passcode page" in {
            test(true, None, routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode)
          }

          "the page is reached via Email passcode confirmation expired" in {
            test(
              true,
              PasscodeVerificationResult.Expired.some,
              routes.VerificationPasscodeExpiredController.verificationPasscodeExpired
            )
          }

          "the page is reached via Too many passcode confirmation attempts" in {
            test(
              true,
              PasscodeVerificationResult.TooManyAttempts.some,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification
            )
          }

        }

        "verify passcode by resend email confirmation " in {

          val session                                     = Fixtures.individualHECSession(
            individualLoginData.copy(emailAddress = ggEmailId.some),
            IndividualRetrievedJourneyData.empty,
            Fixtures.completeIndividualUserAnswers(
              licenceType = DriverOfTaxisAndPrivateHires,
              licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
              licenceValidityPeriod = UpToOneYear,
              taxSituation = PAYE,
              saIncomeDeclared = Some(YesNoAnswer.Yes),
              entityType = Some(Individual)
            ),
            Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
            Some(taxCheckStartDateTime),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            hasResentEmailConfirmation = true,
            userEmailAnswers = Fixtures.userEmailAnswers(passcodeRequestResult = PasscodeSent.some).some
          )
          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.previous(
            routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode
          )
          result shouldBe routes.ResendEmailConfirmationController.resendEmail

        }

        def previousIsConfirmEmailPage(passcodeRequestResult: PasscodeRequestResult, existingRoute: Call) = {
          val session = Fixtures.individualHECSession(
            individualLoginData.copy(emailAddress = ggEmailId.some),
            IndividualRetrievedJourneyData.empty,
            Fixtures.completeIndividualUserAnswers(
              licenceType = DriverOfTaxisAndPrivateHires,
              licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
              licenceValidityPeriod = UpToOneYear,
              taxSituation = PAYE,
              saIncomeDeclared = Some(YesNoAnswer.Yes),
              entityType = Some(Individual)
            ),
            Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now())),
            Some(taxCheckStartDateTime),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            userEmailAnswers = Fixtures.userEmailAnswers(EmailType.GGEmail, ggEmailId, passcodeRequestResult.some).some
          )

          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.previous(existingRoute)
          result shouldBe routes.ConfirmEmailAddressController.confirmEmailAddress
        }

        def previousIsVerificationEmailPage(
          passcodeVerificationResult: PasscodeVerificationResult,
          currentCall: Call,
          resendFlag: Boolean = false
        ) = {
          val session = Fixtures.individualHECSession(
            individualLoginData.copy(emailAddress = ggEmailId.some),
            IndividualRetrievedJourneyData.empty,
            Fixtures.completeIndividualUserAnswers(
              licenceType = DriverOfTaxisAndPrivateHires,
              licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
              licenceValidityPeriod = UpToOneYear,
              taxSituation = PAYE,
              saIncomeDeclared = Some(YesNoAnswer.Yes),
              entityType = Some(Individual)
            ),
            Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
            Some(taxCheckStartDateTime),
            emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
            hasResentEmailConfirmation = resendFlag,
            userEmailAnswers = Fixtures
              .userEmailAnswers(
                passcodeRequestResult = PasscodeSent.some,
                passcode = Passcode("HHHHHH").some,
                passcodeVerificationResult = passcodeVerificationResult.some
              )
              .some
          )

          implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

          val result = journeyService.previous(currentCall)
          if (resendFlag)
            result    shouldBe routes.VerifyResentEmailPasscodeController.verifyResentEmailPasscode
          else result shouldBe routes.VerifyEmailPasscodeController.verifyEmailPasscode

        }

        "the Verify email passcode controller" in {
          previousIsConfirmEmailPage(PasscodeSent, routes.VerifyEmailPasscodeController.verifyEmailPasscode)

        }

        "the Email address confirmed page" when {

          "page is reached not via resend email confirmation and " when {

            "user selected an email on confirm email page, which is already verified" in {
              previousIsConfirmEmailPage(
                EmailAddressAlreadyVerified,
                routes.EmailAddressConfirmedController.emailAddressConfirmed
              )
            }

            "user selected an email which was not confirmed already , but got it confirmed by verifying passcode" in {
              previousIsVerificationEmailPage(
                PasscodeVerificationResult.Match,
                routes.EmailAddressConfirmedController.emailAddressConfirmed
              )
            }
          }

          "page is reached via resend email confirmation and " when {

            "user selected an email which was not confirmed already , but got it confirmed by verifying passcode" in {
              previousIsVerificationEmailPage(
                PasscodeVerificationResult.Match,
                routes.EmailAddressConfirmedController.emailAddressConfirmed,
                true
              )
            }
          }

        }

        "the passcode has  expired page" when {

          "the page is not reached via resent journey" in {
            previousIsVerificationEmailPage(
              PasscodeVerificationResult.Expired,
              routes.VerificationPasscodeExpiredController.verificationPasscodeExpired
            )
          }

          "the page is  reached via resent journey" in {
            previousIsVerificationEmailPage(
              PasscodeVerificationResult.Expired,
              routes.VerificationPasscodeExpiredController.verificationPasscodeExpired,
              true
            )
          }

        }

        "the Too many passcode attempts page" when {

          "the page is not reached via resent journey" in {
            previousIsVerificationEmailPage(
              PasscodeVerificationResult.TooManyAttempts,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification
            )
          }

          "the page is reached via resent journey" in {
            previousIsVerificationEmailPage(
              PasscodeVerificationResult.TooManyAttempts,
              routes.TooManyPasscodeVerificationController.tooManyPasscodeVerification,
              true
            )
          }

        }

        "the too many email verification attempt page" when {

          def test(emailAddress: Option[EmailAddress], hasResentEmailConfirmation: Boolean, previousRoute: Call) = {
            val session = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = emailAddress),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
              hasResentEmailConfirmation = hasResentEmailConfirmation,
              userEmailAnswers = Fixtures
                .userEmailAnswers(passcodeRequestResult = PasscodeRequestResult.MaximumNumberOfEmailsExceeded.some)
                .some
            )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(
              routes.TooManyEmailVerificationAttemptController.tooManyEmailVerificationAttempts
            )
            result shouldBe previousRoute
          }

          "there is email Id in ggAccount" when {

            "page is achieved via confirm email address page " in {
              test(ggEmailId.some, false, routes.ConfirmEmailAddressController.confirmEmailAddress)
            }

            "page is achieved via resend  email confirmation page " in {
              test(ggEmailId.some, true, routes.ResendEmailConfirmationController.resendEmail)
            }
          }

          "there is no email Id in ggAccount" when {

            "page is achieved via enter email address page " in {
              test(None, false, routes.EnterEmailAddressController.enterEmailAddress)
            }

            "page is achieved via resend  email confirmation page " in {
              test(None, true, routes.ResendEmailConfirmationController.resendEmail)
            }
          }

        }

        "the cannot send verification passcode email page" when {

          def test(emailAddress: Option[EmailAddress], previousRoute: Call) = {
            val session = Fixtures.individualHECSession(
              individualLoginData.copy(emailAddress = emailAddress),
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                licenceType = DriverOfTaxisAndPrivateHires,
                licenceTimeTrading = LicenceTimeTrading.TwoToFourYears,
                licenceValidityPeriod = UpToOneYear,
                taxSituation = PAYE,
                saIncomeDeclared = Some(YesNoAnswer.Yes),
                entityType = Some(Individual)
              ),
              Some(HECTaxCheck(HECTaxCheckCode("code"), LocalDate.now.plusDays(1), ZonedDateTime.now)),
              Some(taxCheckStartDateTime),
              emailRequestedForTaxCheck = Fixtures.emailRequestedForTaxCheck().some,
              userEmailAnswers = Fixtures
                .userEmailAnswers(passcodeRequestResult = PasscodeRequestResult.BadEmailAddress.some)
                .some
            )

            implicit val request: RequestWithSessionData[_] = requestWithSessionData(session)

            val result = journeyService.previous(
              routes.CannotSendVerificationPasscodeController.cannotSendVerificationPasscode
            )
            result shouldBe previousRoute
          }

          "there is email Id in ggAccount" in {
            test(ggEmailId.some, routes.ConfirmEmailAddressController.confirmEmailAddress)
          }

          "there is no email Id in ggAccount" in {
            test(None, routes.EnterEmailAddressController.enterEmailAddress)

          }

        }

        def buildIndividualSession(taxSituation: TaxSituation, saStatus: SAStatus): HECSession = {
          val individualLoginData =
            IndividualLoginData(
              GGCredId(""),
              NINO(""),
              Some(SAUTR("utr")),
              Name("", ""),
              DateOfBirth(LocalDate.now()),
              None,
              None
            )

          val journeyData =
            IndividualRetrievedJourneyData(saStatus =
              Some(individual.SAStatusResponse(SAUTR(""), TaxYear(2020), saStatus))
            )

          Fixtures.individualHECSession(
            individualLoginData,
            journeyData,
            Fixtures.incompleteIndividualUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToOneYear),
              Some(taxSituation),
              Some(YesNoAnswer.Yes)
            )
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
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                completeAnswers
              )
            )

          val result = journeyService.previous(
            routes.LicenceDetailsController.licenceType
          )

          result shouldBe routes.CheckYourAnswersController.checkYourAnswers
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

        "entity type is present when it shouldn't be" when {

          "the user did not have to confirm an uncertain entity type previously and " +
            "the licence type does not require an entity type" in {
              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(entityType = Some(EntityType.Individual)),
                IndividualHECSession.newSession(individualLoginData)
              ) shouldBe false

              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(entityType = Some(EntityType.Individual)),
                IndividualHECSession.newSession(individualLoginData.copy(didConfirmUncertainEntityType = Some(false)))
              ) shouldBe false
            }

          "the user has already confirmed an uncertain entity type previously" in {
            JourneyServiceImpl.allIndividualAnswersComplete(
              incompleteUserAnswers = incompleteAnswersBase
                .copy(
                  licenceType = Some(LicenceType.ScrapMetalMobileCollector),
                  entityType = Some(EntityType.Individual)
                ),
              IndividualHECSession.newSession(individualLoginData.copy(didConfirmUncertainEntityType = Some(true)))
            ) shouldBe false
          }
        }

        "tax situation contains SA and" when {

          def test(f: TaxSituation => Unit) =
            List(
              TaxSituation.SA,
              TaxSituation.SAPAYE
            ).foreach { taxSituation =>
              withClue(s"for $taxSituation") {
                f(taxSituation)
              }
            }

          "status = ReturnFound & income declared is missing)" in {
            test { taxSituation =>
              val journeyData = IndividualRetrievedJourneyData(
                saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )

              val session =
                IndividualHECSession
                  .newSession(
                    individualLoginData.copy(sautr = Some(SAUTR("utr")))
                  )
                  .copy(retrievedJourneyData = journeyData)

              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = None
                ),
                session
              ) shouldBe false
            }
          }

          "there is no SA UTR for the user in session" in {
            test { taxSituation =>
              val session =
                IndividualHECSession.newSession(individualLoginData.copy(sautr = None))

              JourneyServiceImpl.allIndividualAnswersComplete(
                incompleteUserAnswers = incompleteAnswersBase.copy(
                  taxSituation = Some(taxSituation),
                  saIncomeDeclared = None
                ),
                session
              ) shouldBe false
            }

          }

          "the SA status response in session says no return was found" in {
            test { taxSituation =>
              val journeyData = IndividualRetrievedJourneyData(
                saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoReturnFound))
              )

              val session =
                IndividualHECSession
                  .newSession(
                    individualLoginData.copy(sautr = Some(SAUTR("utr")))
                  )
                  .copy(retrievedJourneyData = journeyData)

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

        "entity type is not present when it's not needed" in {
          JourneyServiceImpl.allIndividualAnswersComplete(
            incompleteAnswersBase,
            IndividualHECSession.newSession(individualLoginData)
          ) shouldBe true
        }

        "entity type is present when it's needed" when {

          "the user has not had to confirm an uncertain entity type previously and the licence " +
            "type requires an entity type" in {
              val answers = incompleteAnswersBase.copy(
                entityType = Some(EntityType.Individual),
                licenceType = Some(LicenceType.ScrapMetalMobileCollector)
              )

              JourneyServiceImpl.allIndividualAnswersComplete(
                answers,
                IndividualHECSession.newSession(individualLoginData)
              ) shouldBe true

              JourneyServiceImpl.allIndividualAnswersComplete(
                answers,
                IndividualHECSession.newSession(individualLoginData.copy(didConfirmUncertainEntityType = Some(false)))
              ) shouldBe true
            }
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

        "tax situation = SA & status = NoticeToFileIssued (irrespective of whether income declared is missing or present)" in {
          val saTaxSituations = List(
            TaxSituation.SA,
            TaxSituation.SAPAYE
          )

          saTaxSituations foreach { taxSituation =>
            withClue(s"for $taxSituation ") {
              val journeyData =
                IndividualRetrievedJourneyData(
                  saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.NoticeToFileIssued))
                )

              val session =
                IndividualHECSession
                  .newSession(individualLoginData.copy(sautr = Some(SAUTR("utr"))))
                  .copy(retrievedJourneyData = journeyData)

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
                saStatus = Some(individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
              )

              val session =
                IndividualHECSession
                  .newSession(individualLoginData.copy(sautr = Some(SAUTR("utr"))))
                  .copy(retrievedJourneyData = journeyData)

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
        licenceType: Some[LicenceType] = Some(LicenceType.ScrapMetalDealerSite),
        entityType: Option[EntityType] = Some(EntityType.Company),
        didConfirmUncertainEntityType: Option[Boolean] = Some(true)
      ) = {
        val date                = LocalDate.now()
        val companyData         = companyLoginData.copy(
          ctutr = Some(CTUTR("ctutr")),
          didConfirmUncertainEntityType = didConfirmUncertainEntityType
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
          entityType = entityType,
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

        "entity type is missing when the user has not confirmed an uncertain entity type" in {
          JourneyServiceImpl.allCompanyAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase,
            CompanyHECSession.newSession(companyLoginData.copy(didConfirmUncertainEntityType = Some(false)))
          ) shouldBe false

          JourneyServiceImpl.allCompanyAnswersComplete(
            incompleteUserAnswers = incompleteAnswersBase,
            CompanyHECSession.newSession(companyLoginData.copy(didConfirmUncertainEntityType = None))
          ) shouldBe false
        }

        "recently started trading is not present " when {

          "chargeable for CT answer is Yes, CT status = ReturnFound & CT income declared answer is missing" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              None,
              Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is Yes & CT status = NoReturnFound" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              None,
              Some(CTAccountingPeriodDigital(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is not present" in {
            checkCompanyDataComplete(
              None,
              None,
              None,
              Some(CTAccountingPeriodDigital(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

        }

        "recently started trading is  present and is NO " when {

          "chargeable for CT answer is Yes, CT status = ReturnFound & CT income declared answer is missing" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              Some(YesNoAnswer.No),
              Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is Yes & CT status = NoReturnFound" in {
            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              None,
              Some(YesNoAnswer.No),
              Some(CTAccountingPeriodDigital(date, date, CTStatus.NoReturnFound))
            ) shouldBe false
          }

          "chargeable for CT answer is not present" in {
            checkCompanyDataComplete(
              None,
              None,
              Some(YesNoAnswer.No),
              Some(CTAccountingPeriodDigital(date, date, CTStatus.NoReturnFound))
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
                  Some(CTAccountingPeriodDigital(date, date, ctStatus)),
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
              Some(CTAccountingPeriodDigital(date, date, CTStatus.NoticeToFileIssued))
            ) shouldBe true
          }

          "chargeable for CT answer is Yes, CT status = ReturnFound & CT income declared answer is present" in {

            checkCompanyDataComplete(
              Some(YesNoAnswer.Yes),
              Some(YesNoAnswer.Yes),
              None,
              Some(CTAccountingPeriodDigital(date, date, CTStatus.ReturnFound))
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

        "the entity type is missing but the user has confirmed an uncertain entity type" in {
          checkCompanyDataComplete(
            None,
            None,
            Some(YesNoAnswer.Yes),
            None,
            entityType = None,
            didConfirmUncertainEntityType = Some(true)
          ) shouldBe true
        }

      }
    }

  }

}
