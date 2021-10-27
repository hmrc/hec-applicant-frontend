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

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceValidityPeriod.UpToOneYear
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class LicenceDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider = mock[TimeProvider]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider)
  )

  val controller: LicenceDetailsController = instanceOf[LicenceDetailsController]

  val individualLoginData: IndividualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData: CompanyLoginData =
    CompanyLoginData(GGCredId(""), None, None)

  def mockTimeProviderNow(now: ZonedDateTime) =
    (mockTimeProvider.now _).expects().returning(now)

  "LicenceDetailsController" when {

    "handling requests to the licence type page" must {

      def performAction(): Future[Result] = controller.licenceType(FakeRequest())

      def checkPageDetailsWithNoPreviousAns(session: HECSession, radioTextList: List[String]) = {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceType.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val selectedOptions = doc.select(".govuk-radios__input[checked]")
            selectedOptions.isEmpty shouldBe true

            testRadioButtonOptions(doc, radioTextList)

            doc.select(".govuk-body > .govuk-link").attr("href") shouldBe routes.LicenceDetailsController
              .licenceTypeExit()
              .url

            val form = doc.select("form")
            form
              .attr("action") shouldBe routes.LicenceDetailsController.licenceTypeSubmit().url
          }
        )
      }

      def checkPageDetailsWithPreviousAns(session: HECSession, value: String) = {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceType.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val selectedOptions = doc.select(".govuk-radios__input[checked]")
            selectedOptions.attr("value") shouldBe value

            val form = doc.select("form")
            form
              .attr("action") shouldBe routes.LicenceDetailsController.licenceTypeSubmit().url
          }
        )
      }

      def backUrlTest(session: HECSession) = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceType.title"),
          _.select("#back").attr("href") shouldBe mockPreviousCall.url
        )
      }

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        val allLicenceRadioTexts: List[String] = List(
          s"${messageFromMessageKey("licenceType.driverOfTaxis")}" +
            s" ${messageFromMessageKey("licenceType.driverOfTaxis.hint")}",
          messageFromMessageKey("licenceType.operatorOfPrivateHireVehicles"),
          messageFromMessageKey("licenceType.scrapMetalCollector"),
          messageFromMessageKey("licenceType.scrapMetalDealer")
        )

        "user is Individual" when {

          "the user has not previously answered the question" in {
            val session = IndividualHECSession.newSession(individualLoginData)
            checkPageDetailsWithNoPreviousAns(session, allLicenceRadioTexts)

          }

          "the user has previously answered the question" in {
            val session =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                Fixtures.completeIndividualUserAnswers(
                  LicenceType.DriverOfTaxisAndPrivateHires,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToTwoYears,
                  TaxSituation.SA,
                  YesNoAnswer.Yes,
                  Some(EntityType.Individual)
                )
              )

            checkPageDetailsWithPreviousAns(session, "0")
          }

          "the back location is the start endpoint" in {

            val session = IndividualHECSession.newSession(individualLoginData)
            backUrlTest(session)

          }

        }

        "user is Company" when {

          "the user has not previously answered the question" in {
            val session = CompanyHECSession.newSession(companyLoginData)
            checkPageDetailsWithNoPreviousAns(session, allLicenceRadioTexts.takeRight(3))

          }

          "the user has previously answered the question" in {
            val session =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                Fixtures.completeCompanyUserAnswers(
                  LicenceType.ScrapMetalDealerSite,
                  LicenceTimeTrading.ZeroToTwoYears,
                  LicenceValidityPeriod.UpToTwoYears
                )
              )

            checkPageDetailsWithPreviousAns(session, "2")
          }

          "the back location is the start endpoint" in {
            val session = IndividualHECSession.newSession(individualLoginData)
            backUrlTest(session)
          }

        }

      }

    }

    "handling submits on the licence type page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTypeSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      val now = ZonedDateTime.now()

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = IndividualHECSession.newSession(individualLoginData)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("licenceType" -> Int.MaxValue.toString),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("licenceType" -> "xyz"),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey("licenceType.error.invalid")
          )
        }

      }

      "return an internal server error" when {

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty
          val updatedAnswers =
            IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
          val session        =
            IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              None,
              Some(now),
              List.empty
            )
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType(), session, updatedSession)(
              Left(Error(new Exception))
            )
          }

          status(performAction("licenceType" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        def nextpageRedirectTest(
          session: HECSession,
          updatedSession: HECSession,
          radioIndex: String,
          mockNow: Option[() => Unit]
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockNow.foreach(_())
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType(), session, updatedSession)(
              Right(mockNextCall)
            )
          }

          checkIsRedirect(performAction("licenceType" -> radioIndex), mockNextCall)
        }

        "valid Individual data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = IndividualUserAnswers.empty.copy(
              licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
            )
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                Some(now),
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)
            nextpageRedirectTest(session, updatedSession, "1", None)

          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              YesNoAnswer.Yes,
              Some(EntityType.Individual)
            )
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalMobileCollector))
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))
            nextpageRedirectTest(session, updatedSession, "2", Some(() => mockTimeProviderNow(now)))
          }

          "a tax check start date time is already in session" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              YesNoAnswer.Yes,
              Some(EntityType.Individual)
            )
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalMobileCollector))
            val session        = IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              None,
              Some(now),
              List.empty
            )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))
            nextpageRedirectTest(session, updatedSession, "2", None)
          }

          "the user has not changed the licence type they have already submitted previously" in {
            val answers = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              YesNoAnswer.Yes,
              Some(EntityType.Individual)
            )
            val session =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                Some(now),
                List.empty
              )

            nextpageRedirectTest(session, session, "0", None)

          }

        }

        "valid Company data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = CompanyUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
            )
            val updatedAnswers =
              CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
            val session        =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))
            nextpageRedirectTest(session, updatedSession, "0", Some(() => mockTimeProviderNow(now)))

          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeCompanyUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val updatedAnswers =
              CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalMobileCollector))
            val session        =
              Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))

            nextpageRedirectTest(session, updatedSession, "1", Some(() => mockTimeProviderNow(now)))
          }

          "a tax check start date time is already in session" in {
            val answers        = Fixtures.completeCompanyUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val updatedAnswers =
              CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalMobileCollector))
            val session        =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                answers,
                None,
                taxCheckStartDateTime = Some(now),
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))

            nextpageRedirectTest(session, updatedSession, "1", None)
          }

          "the user has not changed the licence type they have already submitted previously" in {
            val answers = Fixtures.completeCompanyUserAnswers(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val session =
              CompanyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                answers,
                None,
                Some(now),
                List.empty
              )
            nextpageRedirectTest(session, session, "0", None)

          }

        }

      }

    }

    "handling requests to the licence type exit page" must {

      def performAction(): Future[Result] = controller.licenceTypeExit(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {
        val session = IndividualHECSession.newSession(individualLoginData)
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTypeExit(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceTypeExit.title"),
          doc => doc.select("#back").attr("href") shouldBe mockPreviousCall.url
        )

      }

    }

    "handling requests to the licence time trading  page" must {

      def performAction(): Future[Result] = controller.licenceTimeTrading(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTimeTradingSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {
          val session =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.TwoToFourYears,
                LicenceValidityPeriod.UpToThreeYears,
                TaxSituation.SA,
                YesNoAnswer.Yes,
                Some(EntityType.Company)
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "1"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTimeTradingSubmit().url
            }
          )
        }

      }

    }

    "handling submits on the licence time trading page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTimeTradingSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = IndividualHECSession.newSession(individualLoginData)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceTimeTrading" -> Int.MaxValue.toString),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceTimeTrading" -> "xyz"),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey("licenceTimeTrading.error.invalid")
          )
        }

      }

      "return an internal server error" when {

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty
          val updatedAnswers =
            IndividualUserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears))
          val session        =
            IndividualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              None,
              None,
              List.empty
            )
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.LicenceDetailsController.licenceTimeTrading(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }

          status(performAction("licenceTimeTrading" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = IndividualUserAnswers.empty
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.FourToEightYears))
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceTimeTrading" -> "2"), mockNextCall)
          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToFiveYears,
              TaxSituation.SA,
              YesNoAnswer.Yes,
              Some(EntityType.Company)
            )
            val updatedAnswers = Fixtures.incompleteIndividualUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceTimeTrading.EightYearsOrMore),
              Some(LicenceValidityPeriod.UpToFiveYears),
              Some(TaxSituation.SA),
              Some(YesNoAnswer.Yes),
              Some(EntityType.Company)
            )
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceTimeTrading" -> "3"), mockNextCall)
          }
        }

      }

    }

    "handling requests to the licence validity period page" must {

      def performAction(): Future[Result] = controller.recentLicenceLength(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "return an InternalServerError" when {

        "a licence type cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "display the page" when {

        "the user has selected a licence type of 'operator of private hire vehicles'" in {
          val session = IndividualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
            ),
            None,
            None,
            List.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceValidityPeriod.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val options = doc.select(".govuk-radios__item")
              options.size() shouldBe 5

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.recentLicenceLengthSubmit().url
            }
          )

        }

        "the user has selected a licence type which isn't 'Operator of Private Hire Vehicles'" in {
          List(
            LicenceType.DriverOfTaxisAndPrivateHires,
            LicenceType.ScrapMetalMobileCollector,
            LicenceType.ScrapMetalDealerSite
          ).foreach { licenceType =>
            withClue(s"For licence type $licenceType: ") {
              val session =
                Fixtures.individualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  Fixtures.completeIndividualUserAnswers(
                    licenceType,
                    LicenceTimeTrading.TwoToFourYears,
                    LicenceValidityPeriod.UpToThreeYears,
                    TaxSituation.SA,
                    YesNoAnswer.Yes,
                    Some(EntityType.Individual)
                  )
                )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), session)(
                  mockPreviousCall
                )
              }

              checkPageIsDisplayed(
                performAction(),
                messageFromMessageKey("licenceValidityPeriod.title"),
                { doc =>
                  doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                  val options = doc.select(".govuk-radios__item")
                  options.size() shouldBe 3

                  val selectedOptions = doc.select(".govuk-radios__input[checked]")
                  selectedOptions.attr("value") shouldBe "2"

                  val form = doc.select("form")
                  form
                    .attr("action") shouldBe routes.LicenceDetailsController.recentLicenceLengthSubmit().url
                }
              )
            }
          }

        }

      }

    }

    "handling submits on the licence time validity period page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.recentLicenceLengthSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "return an InternalServerError" when {

        "a licence type cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
          val updatedAnswers = IndividualUserAnswers.empty
            .copy(licenceType = Some(DriverOfTaxisAndPrivateHires), licenceValidityPeriod = Some(UpToOneYear))
          val session        =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers
            )
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.LicenceDetailsController.recentLicenceLength(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }

          status(performAction("licenceValidityPeriod" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "show a form error" when {

        val answers        = IndividualUserAnswers.empty
        val updatedAnswers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val session        =
          Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            answers
          )
        val updatedSession = session.copy(userAnswers = updatedAnswers)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceValidityPeriod" -> Int.MaxValue.toString),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("licenceValidityPeriod" -> "xyz"),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey("licenceValidityPeriod.error.invalid")
          )
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
            val updatedAnswers = answers.copy(licenceValidityPeriod = Some(UpToOneYear))
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.recentLicenceLength(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceValidityPeriod" -> "0"), mockNextCall)

          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToThreeYears,
              TaxSituation.SA,
              YesNoAnswer.Yes,
              Some(EntityType.Individual)
            )
            val updatedAnswers = Fixtures.incompleteIndividualUserAnswers(
              Some(LicenceType.OperatorOfPrivateHireVehicles),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToFiveYears),
              Some(TaxSituation.SA),
              Some(YesNoAnswer.Yes),
              Some(EntityType.Individual)
            )
            val session        =
              IndividualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                None,
                None,
                List.empty
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.recentLicenceLength(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceValidityPeriod" -> "4"), mockNextCall)
          }
        }

      }

    }

  }

}
