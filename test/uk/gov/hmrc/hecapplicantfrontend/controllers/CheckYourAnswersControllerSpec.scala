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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.EitherT
import cats.instances.future.*
import play.api.inject.bind
import play.api.mvc.Cookie
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.CheckYourAnswersControllerSpec.*
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.getTaxPeriodStrings
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.IndividualHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.*
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTAccountingPeriod.CTAccountingPeriodDigital
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.{CTAccountingPeriod, CTStatus, CTStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.{LocalDate, ZoneId, ZonedDateTime}
import scala.jdk.CollectionConverters.*
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
    Fixtures.individualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData = Fixtures.companyLoginData(ctutr = Some(CTUTR("1111111111")))

  def mockSaveTaxCheck(
    HECSession: HECSession,
    completeAnswers: CompleteUserAnswers,
    language: Language
  )(
    result: Either[Error, HECTaxCheck]
  ) =
    (mockTaxCheckService
      .saveTaxCheck(_: HECSession, _: CompleteUserAnswers, _: Language)(_: HeaderCarrier))
      .expects(HECSession, completeAnswers, language, *)
      .returning(EitherT.fromEither(result))

  "CheckYourAnswersController" when {

    "handling requests to display the check your answers page" must {

      val chargeableForCTTitleDate = LocalDate.of(2021, 10, 9)

      def expectedIndividualRows(startDate: String, endDate: String): List[CheckYourAnswersRow] = List(
        CheckYourAnswersRow(
          messageFromMessageKey("licenceType.title"),
          messageFromMessageKey("licenceType.scrapMetalCollector"),
          routes.LicenceDetailsController.licenceType.url
        ),
        CheckYourAnswersRow(
          messageFromMessageKey("licenceTimeTrading.title"),
          messageFromMessageKey("licenceTimeTrading.zeroToTwoYears"),
          routes.LicenceDetailsController.licenceTimeTrading.url
        ),
        CheckYourAnswersRow(
          messageFromMessageKey("licenceValidityPeriod.title"),
          messageFromMessageKey("licenceValidityPeriod.upToTwoYears"),
          routes.LicenceDetailsController.recentLicenceLength.url
        ),
        CheckYourAnswersRow(
          messageFromMessageKey("entityType.title"),
          messageFromMessageKey("entityType.individual"),
          routes.EntityTypeController.entityType.url
        ),
        CheckYourAnswersRow(
          messageFromMessageKey("taxSituation.title", startDate, endDate),
          messageFromMessageKey("taxSituation.PA"),
          routes.TaxSituationController.determineIfRelevantIncomeTaxYearChanged.url
        ),
        CheckYourAnswersRow(
          messageFromMessageKey("saIncomeDeclared.title"),
          messageFromMessageKey("saIncomeDeclared.yes"),
          routes.SAController.saIncomeStatement.url
        )
      )

      val companyExpectedRows: List[CheckYourAnswersRow] =
        List(
          CheckYourAnswersRow(
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.scrapMetalCollector"),
            routes.LicenceDetailsController.licenceType.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.zeroToTwoYears"),
            routes.LicenceDetailsController.licenceTimeTrading.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.upToTwoYears"),
            routes.LicenceDetailsController.recentLicenceLength.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("entityType.title"),
            messageFromMessageKey("entityType.company"),
            routes.EntityTypeController.entityType.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("crn.title"),
            "1123456",
            routes.CRNController.companyRegistrationNumber.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("enterCtutr.title"),
            "1111111111",
            routes.CompanyDetailsController.enterCtutr.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey(
              "chargeableForCT.title",
              TimeUtils.govDisplayFormat(chargeableForCTTitleDate)
            ),
            messageFromMessageKey("chargeableForCT.yes"),
            routes.CompanyDetailsController.determineIfRelevantAccountingPeriodChanged.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("ctIncomeDeclared.title"),
            messageFromMessageKey("ctIncomeDeclared.yes"),
            routes.CompanyDetailsController.ctIncomeStatement.url
          ),
          CheckYourAnswersRow(
            messageFromMessageKey("recentlyStartedTrading.title"),
            messageFromMessageKey("recentlyStartedTrading.yes"),
            routes.CompanyDetailsController.determineIfRelevantAccountingPeriodChanged.url
          )
        )

      def performAction() = controller.checkYourAnswers(FakeRequest())

      behave like (authAndSessionDataBehaviour(() => performAction()))

      "show an error page" when {

        "there are no complete answers in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

      }

      "display the page" when {

        def test(
          session: HECSession,
          isIndividual: Boolean,
          isRecentlyStartedTrading: Boolean,
          isCTUTRPresent: Boolean
        ) = {

          val didConfirmUncertainEntityType = session.loginData.didConfirmUncertainEntityType.contains(true)

          val confirmUncertainEntityTypeRow =
            CheckYourAnswersRow(
              messageFromMessageKey("entityType.title"),
              messageFromMessageKey(s"entityType.${if (isIndividual) "individual" else "company"}"),
              routes.ConfirmUncertainEntityTypeController.entityType.url
            )

          val excludedChangeTitleList1 = List(
            messageFromMessageKey("enterCtutr.title"),
            messageFromMessageKey("recentlyStartedTrading.title")
          )

          val excludedChangeTitleList2 = List(
            messageFromMessageKey("enterCtutr.title"),
            messageFromMessageKey(
              "chargeableForCT.title",
              TimeUtils.govDisplayFormat(chargeableForCTTitleDate)
            ),
            messageFromMessageKey("ctIncomeDeclared.title")
          )

          val expectedRows = {
            val rows = if (isIndividual) {
              val (startDate, endDate) = getTaxPeriodStrings(TaxYear(2020))
              expectedIndividualRows(startDate, endDate)
            } else {
              (isRecentlyStartedTrading, isCTUTRPresent) match {
                case (_, true)  =>
                  companyExpectedRows.filterNot(
                    _.question === messageFromMessageKey("recentlyStartedTrading.title")
                  )
                case (true, _)  => companyExpectedRows.filterNot(row => excludedChangeTitleList2.contains(row.question))
                case (false, _) => companyExpectedRows.filterNot(row => excludedChangeTitleList1.contains(row.question))
                case _          => companyExpectedRows
              }
            }

            if (didConfirmUncertainEntityType)
              confirmUncertainEntityTypeRow :: rows.filterNot(
                _.changeUrl === routes.EntityTypeController.entityType.url
              )
            else
              rows
          }

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers, session)(
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

        "applicant is an Individual and" when {

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

          "the user did not confirm an uncertain entity type" in {
            test(session, true, false, false)
          }

          "the user did confirm an uncertain entity type" in {
            test(
              session.copy(
                loginData = individualLoginData.copy(didConfirmUncertainEntityType = Some(true)),
                userAnswers = answers.copy(entityType = None)
              ),
              true,
              false,
              false
            )
          }
        }

        "applicant is a Company and " when {

          def createCompleteAnswers(
            chargeableForCTOpt: Option[YesNoAnswer],
            ctIncomeDeclaredOpt: Option[YesNoAnswer],
            recentlyStartedTrading: Option[YesNoAnswer],
            ctutr: Option[CTUTR] = None,
            entityType: Option[EntityType] = Some(EntityType.Company)
          ) = Fixtures.completeCompanyUserAnswers(
            LicenceType.ScrapMetalMobileCollector,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToTwoYears,
            entityType = entityType,
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

          "company has not recently started trading" in {

            val answers = createCompleteAnswers(Some(YesNoAnswer.Yes), Some(YesNoAnswer.Yes), None)

            val session =
              Fixtures.companyHECSession(
                companyLoginData,
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    Some(
                      CTAccountingPeriodDigital(
                        LocalDate.of(2020, 10, 9),
                        LocalDate.of(2021, 10, 9),
                        CTStatus.ReturnFound
                      )
                    )
                  )
                ),
                answers
              )

            test(session, false, false, false)
          }

          "company has recently started trading" in {

            val answers = createCompleteAnswers(None, None, Some(YesNoAnswer.Yes))

            val session =
              Fixtures.companyHECSession(
                companyLoginData,
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    None
                  )
                ),
                answers
              )

            test(session, false, true, false)

          }

          "company's ctutr is present in user answers" in {
            val answers =
              createCompleteAnswers(Some(YesNoAnswer.Yes), Some(YesNoAnswer.Yes), None, Some(CTUTR("1111111111")))

            val session =
              Fixtures.companyHECSession(
                companyLoginData,
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    Some(
                      CTAccountingPeriodDigital(
                        LocalDate.of(2020, 10, 9),
                        LocalDate.of(2021, 10, 9),
                        CTStatus.ReturnFound
                      )
                    )
                  )
                ),
                answers
              )

            test(session, false, false, true)
          }

          "the user confirmed an uncertain entity type" in {
            val answers =
              createCompleteAnswers(Some(YesNoAnswer.Yes), Some(YesNoAnswer.Yes), None, Some(CTUTR("1111111111")), None)

            val session =
              Fixtures.companyHECSession(
                companyLoginData.copy(didConfirmUncertainEntityType = Some(true)),
                Fixtures.companyRetrievedJourneyData(
                  companyName = Some(CompanyHouseName("Test Tech Ltd")),
                  desCtutr = Some(CTUTR("1111111111")),
                  ctStatus = createCTStatus(
                    Some(
                      CTAccountingPeriodDigital(
                        LocalDate.of(2020, 10, 9),
                        LocalDate.of(2021, 10, 9),
                        CTStatus.ReturnFound
                      )
                    )
                  )
                ),
                answers
              )

            test(session, false, false, true)
          }
        }

      }

    }

    "handling submits on the check your answers page" must {

      def performAction(language: Language) =
        controller.checkYourAnswersSubmit(
          FakeRequest().withMethod(POST).withCookies(Cookie("PLAY_LANG", language.code))
        )

      behave like authAndSessionDataBehaviour(() => performAction(Language.English))

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

      val hecTaxCheck = HECTaxCheck(HECTaxCheckCode(""), LocalDate.now(), ZonedDateTime.now())

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
          assertThrows[InconsistentSessionState](await(performAction(Language.English)))

        }

        "there is an error saving the tax check" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session, completeAnswers, Language.English)(
              Left(Error(new Exception("Oh no!")))
            )
          }
          assertThrows[RuntimeException](await(performAction(Language.English)))

        }

        "there is an error updating the session" in {

          val updatedSession = session.copy(completedTaxCheck = Some(hecTaxCheck))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session, completeAnswers, Language.English)(Right(hecTaxCheck))
            mockJourneyServiceUpdateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers,
              session,
              updatedSession
            )(Left(Error("")))
          }
          assertThrows[RuntimeException](await(performAction(Language.English)))

        }

      }

      "redirect to the next page" when {

        "the tax check has successfully been saved" in {

          val updatedSession = session.copy(completedTaxCheck = Some(hecTaxCheck))
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session, completeAnswers, Language.Welsh)(Right(hecTaxCheck))
            mockJourneyServiceUpdateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers,
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction(Language.Welsh), mockNextCall)
        }

      }

    }

  }

}

object CheckYourAnswersControllerSpec {

  final case class CheckYourAnswersRow(question: String, answer: String, changeUrl: String)

}
