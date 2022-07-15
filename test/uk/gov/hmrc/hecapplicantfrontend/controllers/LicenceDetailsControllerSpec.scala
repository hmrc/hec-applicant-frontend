/*
 * Copyright 2022 HM Revenue & Customs
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
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)

  val companyLoginData: CompanyLoginData =
    CompanyLoginData(GGCredId(""), None, None, None)

  def mockTimeProviderNow(now: ZonedDateTime) =
    (mockTimeProvider.now _).expects().returning(now)

  "LicenceDetailsController" when {

    "handling requests to the licence type page" must {

      def performAction(): Future[Result] = controller.licenceType(FakeRequest())

      def checkPageDetailsWithNoPreviousAns(session: HECSession, radioTextList: List[String]) = {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType, session)(mockPreviousCall)
        }
        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("licenceType.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val selectedOptions = doc.select(".govuk-radios__input[checked]")
            selectedOptions.isEmpty shouldBe true

            testRadioButtonOptions(doc, radioTextList)

            doc
              .select(".govuk-body > .govuk-link")
              .attr("href") shouldBe routes.LicenceDetailsController.licenceTypeExit.url

            val form = doc.select("form")
            form
              .attr("action") shouldBe routes.LicenceDetailsController.licenceTypeSubmit.url

            doc.select("#back").attr("href") shouldBe mockPreviousCall.url
          }
        )
      }

      def checkPageDetailsWithPreviousAns(session: HECSession, value: String) = {

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType, session)(mockPreviousCall)
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
              .attr("action") shouldBe routes.LicenceDetailsController.licenceTypeSubmit.url

            doc.select("#back").attr("href") shouldBe mockPreviousCall.url
          }
        )
      }

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "user is Individual" when {

          "the user has not previously answered the question" in {
            val session = IndividualHECSession.newSession(individualLoginData)
            checkPageDetailsWithNoPreviousAns(
              session,
              List(
                messageFromMessageKey("licenceType.driverOfTaxis") +
                  s" ${messageFromMessageKey("licenceType.driverOfTaxis.hint")}",
                messageFromMessageKey("licenceType.operatorOfPrivateHireVehicles") +
                  s" ${messageFromMessageKey("licenceType.operatorOfPrivateHireVehicles.hint")}",
                messageFromMessageKey("licenceType.bookingOffice") +
                  s" ${messageFromMessageKey("licenceType.bookingOffice.hint")}",
                messageFromMessageKey("licenceType.scrapMetalCollector") +
                  s" ${messageFromMessageKey("licenceType.scrapMetalCollector.hint")}",
                messageFromMessageKey("licenceType.scrapMetalDealer") +
                  s" ${messageFromMessageKey("licenceType.scrapMetalDealer.hint")}"
              )
            )
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
                  Some(YesNoAnswer.Yes),
                  Some(EntityType.Individual)
                )
              )

            checkPageDetailsWithPreviousAns(session, "0")
          }

        }

        "user is Company" when {

          "the user has not previously answered the question" in {
            val session = CompanyHECSession.newSession(companyLoginData)
            checkPageDetailsWithNoPreviousAns(
              session,
              List(
                messageFromMessageKey("licenceType.operatorOfPrivateHireVehicles") +
                  s" ${messageFromMessageKey("licenceType.operatorOfPrivateHireVehicles.hint")}",
                messageFromMessageKey("licenceType.bookingOffice") +
                  s" ${messageFromMessageKey("licenceType.bookingOffice.hint")}",
                messageFromMessageKey("licenceType.scrapMetalCollector") +
                  s" ${messageFromMessageKey("licenceType.scrapMetalCollector.hint")}",
                messageFromMessageKey("licenceType.scrapMetalDealer") +
                  s" ${messageFromMessageKey("licenceType.scrapMetalDealer.hint")}"
              )
            )
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

            checkPageDetailsWithPreviousAns(session, "3")
          }

        }

      }

    }

    "handling submits on the licence type page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTypeSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      val now = ZonedDateTime.now()

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = IndividualHECSession.newSession(individualLoginData)

        def testFormError(data: (String, String)*)(errorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceType, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey("licenceType.title"),
            messageFromMessageKey(errorMessageKey)
          )
        }

        "nothing is submitted" in {
          testFormError()("licenceType.error.required")
        }

        "an index is submitted which is too large" in {
          testFormError("licenceType" -> Int.MaxValue.toString)("licenceType.error.invalid")
        }

        "a value is submitted which is not a number" in {
          testFormError("licenceType" -> "xyz")("licenceType.error.invalid")
        }

      }

      "return a technical error" when {

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty
          val updatedAnswers =
            IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
          val session        =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              taxCheckStartDateTime = Some(now)
            )
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType, session, updatedSession)(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("licenceType" -> "0")))

        }

      }

      "redirect to the next page" when {

        def nextPageRedirectTest(
          session: HECSession,
          updatedSession: HECSession,
          radioIndex: String,
          mockNow: Option[() => Unit]
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockNow.foreach(_())
            mockJourneyServiceUpdateAndNext(routes.LicenceDetailsController.licenceType, session, updatedSession)(
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
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                taxCheckStartDateTime = Some(now)
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)
            nextPageRedirectTest(session, updatedSession, "1", None)
          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              Some(YesNoAnswer.Yes),
              Some(EntityType.Individual)
            )
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.BookingOffice))
            val session        =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))
            nextPageRedirectTest(session, updatedSession, "2", Some(() => mockTimeProviderNow(now)))
          }

          "a tax check start date time is already in session" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              Some(YesNoAnswer.Yes),
              Some(EntityType.Individual)
            )
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.ScrapMetalMobileCollector))
            val session        = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers,
              taxCheckStartDateTime = Some(now)
            )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))
            nextPageRedirectTest(session, updatedSession, "3", None)
          }

          "the user has not changed the licence type they have already submitted previously" in {
            val answers = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SA,
              Some(YesNoAnswer.Yes),
              Some(EntityType.Individual)
            )
            val session =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers,
                taxCheckStartDateTime = Some(now)
              )

            nextPageRedirectTest(session, session, "0", None)

          }

        }

        "valid Company data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = CompanyUserAnswers.empty
            val updatedAnswers =
              CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
            val session        =
              Fixtures.companyHECSession(
                companyLoginData,
                CompanyRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))

            nextPageRedirectTest(session, updatedSession, "0", Some(() => mockTimeProviderNow(now)))
          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeCompanyUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val updatedAnswers =
              CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.BookingOffice))
            val session        =
              Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers, taxCheckStartDateTime = Some(now))

            nextPageRedirectTest(session, updatedSession, "1", Some(() => mockTimeProviderNow(now)))
          }

          "a tax check start date time is already in session" in {
            val answers        = Fixtures.completeCompanyUserAnswers(
              LicenceType.ScrapMetalMobileCollector,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val updatedAnswers =
              CompanyUserAnswers.empty.copy(licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles))
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

            nextPageRedirectTest(session, updatedSession, "0", None)
          }

          "the user has not changed the licence type they have already submitted previously" in {
            val answers = Fixtures.completeCompanyUserAnswers(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear
            )
            val session = Fixtures.companyHECSession(
              companyLoginData,
              CompanyRetrievedJourneyData.empty,
              answers,
              taxCheckStartDateTime = Some(now)
            )

            nextPageRedirectTest(session, session, "0", None)

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
          mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTypeExit, session)(mockPreviousCall)
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

        def testDisplayPage(session: HECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading, session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("licenceTimeTrading.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.licenceTimeTradingSubmit.url
            }
          )
        }

        "the user has not previously answered the question" in {
          val session = IndividualHECSession.newSession(individualLoginData)
          testDisplayPage(session, None)

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
                Some(YesNoAnswer.Yes),
                Some(EntityType.Company)
              )
            )
          testDisplayPage(session, Some("1"))
        }

      }

    }

    "handling submits on the licence time trading page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.licenceTimeTradingSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = IndividualHECSession.newSession(individualLoginData)

        def testFormError(data: (String, String)*)(errorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.licenceTimeTrading, session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey("licenceTimeTrading.title"),
            messageFromMessageKey(errorMessageKey)
          )
        }

        "nothing is submitted" in {
          testFormError()("licenceTimeTrading.error.required")
        }

        "an index is submitted which is too large" in {
          testFormError("licenceTimeTrading" -> Int.MaxValue.toString)("licenceTimeTrading.error.invalid")
        }

        "a value is submitted which is not a number" in {
          testFormError("licenceTimeTrading" -> "xyz")("licenceTimeTrading.error.invalid")
        }

      }

      "return a technical error" when {

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty
          val updatedAnswers =
            IndividualUserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.ZeroToTwoYears))
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
              routes.LicenceDetailsController.licenceTimeTrading,
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("licenceTimeTrading" -> "0")))

        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          def redirectTest(session: HECSession, updatedSession: HECSession, data: String) = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.licenceTimeTrading,
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceTimeTrading" -> data), mockNextCall)
          }

          "the user has not previously completed answering questions" in {
            val answers        = IndividualUserAnswers.empty
            val updatedAnswers =
              IndividualUserAnswers.empty.copy(licenceTimeTrading = Some(LicenceTimeTrading.FourToEightYears))
            val session        =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)
            redirectTest(session, updatedSession, "2")

          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToFiveYears,
              TaxSituation.SA,
              Some(YesNoAnswer.Yes),
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
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)
            redirectTest(session, updatedSession, "3")

          }
        }

      }

    }

    "handling requests to the licence validity period page" must {

      def performAction(): Future[Result] = controller.recentLicenceLength(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        def displayPageTest(session: HECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength, session)(
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

              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.LicenceDetailsController.recentLicenceLengthSubmit.url
            }
          )
        }

        "the user has not selected an option before" in {
          val session = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(
              licenceType = Some(LicenceType.OperatorOfPrivateHireVehicles)
            )
          )

          displayPageTest(session, None)

        }

        "the user has selected an option before'" in {
          val session =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                LicenceType.BookingOffice,
                LicenceTimeTrading.TwoToFourYears,
                LicenceValidityPeriod.UpToThreeYears,
                TaxSituation.SA,
                Some(YesNoAnswer.Yes),
                Some(EntityType.Individual)
              )
            )

          displayPageTest(session, Some("2"))

        }

      }

    }

    "handling submits on the licence time validity period page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.recentLicenceLengthSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "return a technical error" when {

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
              routes.LicenceDetailsController.recentLicenceLength,
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("licenceValidityPeriod" -> "0")))

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

        def formErrorTest(data: (String, String)*)(errorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.LicenceDetailsController.recentLicenceLength, updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey("licenceValidityPeriod.title"),
            messageFromMessageKey(errorMessageKey)
          )
        }

        "nothing is submitted" in {
          formErrorTest()("licenceValidityPeriod.error.required")
        }

        "an index is submitted which is too large" in {
          formErrorTest("licenceValidityPeriod" -> Int.MaxValue.toString)("licenceValidityPeriod.error.invalid")
        }

        "a value is submitted which is not a number" in {
          formErrorTest("licenceValidityPeriod" -> "xyz")("licenceValidityPeriod.error.invalid")
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          def testRedirect(session: HECSession, updatedSession: HECSession, value: String) = {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.LicenceDetailsController.recentLicenceLength,
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("licenceValidityPeriod" -> value), mockNextCall)

          }

          "the user has not previously completed answering questions" in {
            val answers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              answers
            )

            val updatedAnswers = answers.copy(licenceValidityPeriod = Some(UpToOneYear))
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            testRedirect(session, updatedSession, "0")

          }

          "the user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToThreeYears,
              TaxSituation.SA,
              Some(YesNoAnswer.Yes),
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
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            testRedirect(session, updatedSession, "4")
          }
        }

      }

    }

    "handling request on the max tax check code attempt exceeded for a licence type page" must {
      def performAction(): Future[Result] = controller.maxTaxChecksExceeded(FakeRequest())

      def unexpiredTaxCheckList(licenceType: LicenceType) = List(
        TaxCheckListItem(
          licenceType,
          HECTaxCheckCode("ABC 123 ABC"),
          LocalDate.now().plusDays(1),
          ZonedDateTime.now()
        ),
        TaxCheckListItem(
          licenceType,
          HECTaxCheckCode("ABE 123 ABE"),
          LocalDate.now().plusDays(1),
          ZonedDateTime.now()
        )
      )

      def test(session: HECSession) = {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("maxTaxChecksLimitExceeded.title"),
          { doc =>
            doc.select(".govuk-body").get(0).text shouldBe messageFromMessageKey("maxTaxChecksLimitExceeded.p1")

            val links = doc.select(".govuk-body > .govuk-link")
            links.get(0).attr("href") shouldBe routes.StartController.start.url
            links.get(1).attr("href") shouldBe routes.LicenceDetailsController.licenceType.url
          }
        )
      }

      "display the page" when {
        " user is an Individual" in {
          val session = Fixtures.individualHECSession(
            userAnswers = Fixtures.completeIndividualUserAnswers(),
            unexpiredTaxChecks = unexpiredTaxCheckList(LicenceType.DriverOfTaxisAndPrivateHires)
          )
          test(session)
        }

        " user is a Company" in {
          val session = Fixtures.companyHECSession(
            userAnswers = Fixtures.completeCompanyUserAnswers(),
            unexpiredTaxChecks = unexpiredTaxCheckList(LicenceType.OperatorOfPrivateHireVehicles)
          )
          test(session)
        }
      }
    }

  }

}
