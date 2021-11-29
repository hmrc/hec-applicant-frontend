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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.CheckYourAnswersControllerSpec._
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.getTaxPeriodStrings
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.collection.JavaConverters._
import scala.concurrent.ExecutionContext.Implicits.global

class CheckYourAnswersControllerSpec
    extends ControllerSpec
    with JourneyServiceSupport
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  val mockTaxCheckService = mock[TaxCheckService]

  val zonedDateTimeNow = ZonedDateTime.of(2021, 10, 9, 9, 12, 34, 0, ZoneId.of("Europe/London"))

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TaxCheckService].toInstance(mockTaxCheckService)
  )

  val controller = instanceOf[CheckYourAnswersController]

  val individualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData = Fixtures.companyLoginData(ctutr = Some(CTUTR("1111111111")))

  def mockSaveTaxCheck(
    HECSession: HECSession,
    completeAnswers: CompleteUserAnswers
  )(
    result: Either[Error, HECTaxCheck]
  ) =
    (mockTaxCheckService
      .saveTaxCheck(_: HECSession, _: CompleteUserAnswers)(_: HeaderCarrier))
      .expects(HECSession, completeAnswers, *)
      .returning(EitherT.fromEither(result))

  "CheckYourAnswersController" when {

    "handling requests to display the check your answers page" must {

      def performAction() = controller.checkYourAnswers(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "show an error page" when {

        "there are no complete answers in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[RuntimeException](await(performAction()))
        }

        "there are complete individual answers but a relevant income tax year cannot be found" in {
          val answers = Fixtures.completeIndividualUserAnswers()

          val session =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              relevantIncomeTaxYear = None
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers(), session)(
              mockPreviousCall
            )
          }

          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "display the page" when {

        "applicant is an Individual" in {
          val answers = Fixtures.completeIndividualUserAnswers(
            LicenceType.ScrapMetalMobileCollector,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToTwoYears,
            TaxSituation.PAYE,
            Some(YesNoAnswer.Yes),
            Some(EntityType.Individual)
          )

          val session =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              relevantIncomeTaxYear = Some(TaxYear(2020))
            )

          val (startDate, endDate) = getTaxPeriodStrings(TaxYear(2020))

          val expectedRows =
            List(
              CheckYourAnswersRow(
                messageFromMessageKey("licenceType.title"),
                messageFromMessageKey("licenceType.scrapMetalCollector"),
                routes.LicenceDetailsController.licenceType().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("licenceTimeTrading.title"),
                messageFromMessageKey("licenceTimeTrading.zeroToTwoYears"),
                routes.LicenceDetailsController.licenceTimeTrading().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("licenceValidityPeriod.title"),
                messageFromMessageKey("licenceValidityPeriod.upToTwoYears"),
                routes.LicenceDetailsController.recentLicenceLength().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("entityType.title"),
                messageFromMessageKey("entityType.individual"),
                routes.EntityTypeController.entityType().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("taxSituation.title", startDate, endDate),
                messageFromMessageKey("taxSituation.PA"),
                routes.TaxSituationController.taxSituation().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("saIncomeDeclared.title"),
                messageFromMessageKey("saIncomeDeclared.yes"),
                routes.SAController.saIncomeStatement().url
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("checkYourAnswers.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val rows =
                doc.select(".govuk-summary-list__row").iterator().asScala.toList.map { element =>
                  val question  = element.select(".govuk-summary-list__key").text()
                  val answer    = element.select(".govuk-summary-list__value").text()
                  val changeUrl = element.select(".govuk-link").attr("href")
                  CheckYourAnswersRow(question, answer, changeUrl)
                }

              rows shouldBe expectedRows
            }
          )
        }

        "applicant is a Company and " when {

          def createCompleteAnswers(
            chargeableForCTOpt: Option[YesNoAnswer],
            ctIncomeDeclaredOpt: Option[YesNoAnswer],
            recentlyStartedTrading: Option[YesNoAnswer],
            ctutr: Option[CTUTR] = None
          ) = Fixtures.completeCompanyUserAnswers(
            LicenceType.ScrapMetalMobileCollector,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToTwoYears,
            entityType = EntityType.Company,
            crn = CRN("1123456"),
            ctutr = ctutr,
            companyDetailsConfirmed = YesNoAnswer.Yes,
            chargeableForCT = chargeableForCTOpt,
            ctIncomeDeclared = ctIncomeDeclaredOpt,
            recentlyStartedTrading = recentlyStartedTrading
          )

          def createCTStatus(latestAccountingPeriod: Option[CTAccountingPeriod]) = Some(
            CTStatusResponse(
              ctutr = CTUTR("1111111111"),
              startDate = LocalDate.of(2020, 10, 9),
              endDate = LocalDate.of(2021, 10, 9),
              latestAccountingPeriod = latestAccountingPeriod
            )
          )

          val expectedRows: List[CheckYourAnswersRow] =
            List(
              CheckYourAnswersRow(
                messageFromMessageKey("licenceType.title"),
                messageFromMessageKey("licenceType.scrapMetalCollector"),
                routes.LicenceDetailsController.licenceType().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("licenceTimeTrading.title"),
                messageFromMessageKey("licenceTimeTrading.zeroToTwoYears"),
                routes.LicenceDetailsController.licenceTimeTrading().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("licenceValidityPeriod.title"),
                messageFromMessageKey("licenceValidityPeriod.upToTwoYears"),
                routes.LicenceDetailsController.recentLicenceLength().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("entityType.title"),
                messageFromMessageKey("entityType.company"),
                routes.EntityTypeController.entityType().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("crn.title"),
                "1123456",
                routes.CRNController.companyRegistrationNumber().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("enterCtutr.title"),
                "1111111111",
                routes.CompanyDetailsController.enterCtutr().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey(
                  "chargeableForCT.title",
                  TimeUtils.govDisplayFormat(LocalDate.of(2021, 10, 9))
                ),
                messageFromMessageKey("chargeableForCT.yes"),
                routes.CompanyDetailsController.chargeableForCorporationTax().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("ctIncomeDeclared.title"),
                messageFromMessageKey("ctIncomeDeclared.yes"),
                routes.CompanyDetailsController.ctIncomeStatement().url
              ),
              CheckYourAnswersRow(
                messageFromMessageKey("recentlyStartedTrading.title"),
                messageFromMessageKey("recentlyStartedTrading.yes"),
                routes.CompanyDetailsController.recentlyStartedTrading().url
              )
            )

          "company has not recently started trading" in {

            val answers = createCompleteAnswers(Some(YesNoAnswer.Yes), Some(YesNoAnswer.Yes), None)

            val session =
              CompanyHECSession(
                companyLoginData,
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    Some(
                      CTAccountingPeriod(LocalDate.of(2020, 10, 9), LocalDate.of(2021, 10, 9), CTStatus.ReturnFound)
                    )
                  )
                ),
                answers,
                None,
                None,
                List.empty
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers(), session)(
                mockPreviousCall
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("checkYourAnswers.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                val rows =
                  doc.select(".govuk-summary-list__row").iterator().asScala.toList.map { element =>
                    val question  = element.select(".govuk-summary-list__key").text()
                    val answer    = element.select(".govuk-summary-list__value").text()
                    val changeUrl = element.select(".govuk-link").attr("href")
                    CheckYourAnswersRow(question, answer, changeUrl)
                  }

                rows shouldBe (expectedRows.take(5) ::: expectedRows.slice(6, 8))
              }
            )

          }

          "company has  recently started trading" in {

            val answers = createCompleteAnswers(None, None, Some(YesNoAnswer.Yes))

            val session =
              CompanyHECSession(
                companyLoginData,
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    None
                  )
                ),
                answers,
                None,
                None,
                List.empty
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers(), session)(
                mockPreviousCall
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("checkYourAnswers.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                val rows =
                  doc.select(".govuk-summary-list__row").iterator().asScala.toList.map { element =>
                    val question  = element.select(".govuk-summary-list__key").text()
                    val answer    = element.select(".govuk-summary-list__value").text()
                    val changeUrl = element.select(".govuk-link").attr("href")
                    CheckYourAnswersRow(question, answer, changeUrl)
                  }

                rows shouldBe (expectedRows.take(5) ::: expectedRows.takeRight(1))
              }
            )

          }

          "company's ctutr is present in user answers" in {
            val answers =
              createCompleteAnswers(Some(YesNoAnswer.Yes), Some(YesNoAnswer.Yes), None, Some(CTUTR("1111111111")))

            val session =
              CompanyHECSession(
                companyLoginData,
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    Some(
                      CTAccountingPeriod(LocalDate.of(2020, 10, 9), LocalDate.of(2021, 10, 9), CTStatus.ReturnFound)
                    )
                  )
                ),
                answers,
                None,
                None,
                List.empty
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers(), session)(
                mockPreviousCall
              )
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("checkYourAnswers.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                val rows =
                  doc.select(".govuk-summary-list__row").iterator().asScala.toList.map { element =>
                    val question  = element.select(".govuk-summary-list__key").text()
                    val answer    = element.select(".govuk-summary-list__value").text()
                    val changeUrl = element.select(".govuk-link").attr("href")
                    CheckYourAnswersRow(question, answer, changeUrl)
                  }

                rows shouldBe (expectedRows.take(8))
              }
            )
          }
        }

      }

    }

    "handling submits on the check your answers page" must {

      def performAction() = controller.checkYourAnswersSubmit(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      val completeAnswers = Fixtures.completeIndividualUserAnswers(
        LicenceType.OperatorOfPrivateHireVehicles,
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToOneYear,
        TaxSituation.SA,
        Some(YesNoAnswer.Yes),
        Some(EntityType.Individual)
      )

      val session = Fixtures.individualHECSession(
        loginData = individualLoginData,
        retrievedJourneyData = IndividualRetrievedJourneyData.empty,
        userAnswers = completeAnswers,
        taxCheckStartDateTime = Some(zonedDateTimeNow)
      )

      val hecTaxCheck = HECTaxCheck(HECTaxCheckCode(""), LocalDate.now())

      "return a technical error" when {

        "there are no complete answers in session" in {
          val session = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))

        }

        "there is an error saving the tax check" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session, completeAnswers)(
              Left(Error(new Exception("Oh no!")))
            )
          }
          assertThrows[RuntimeException](await(performAction()))

        }

        "there is an error updating the session" in {

          val updatedSession = session.copy(completedTaxCheck = Some(hecTaxCheck))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session, completeAnswers)(Right(hecTaxCheck))
            mockJourneyServiceUpdateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers(),
              session,
              updatedSession
            )(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction()))

        }

      }

      "redirect to the next page" when {

        "the tax check has successfully been saved" in {

          val updatedSession = session.copy(completedTaxCheck = Some(hecTaxCheck))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session, completeAnswers)(Right(hecTaxCheck))
            mockJourneyServiceUpdateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction(), mockNextCall)
        }

      }

    }

  }

}

object CheckYourAnswersControllerSpec {

  final case class CheckYourAnswersRow(question: String, answer: String, changeUrl: String)

}
