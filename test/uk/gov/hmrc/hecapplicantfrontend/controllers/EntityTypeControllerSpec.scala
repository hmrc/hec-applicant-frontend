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

import play.api.inject.bind
import play.api.mvc.{Call, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers.*
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.CompanyLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.*
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.{SessionStore, UncertainEntityTypeJourneyStore}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EntityTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport
    with UncertainEntityTypeJourneyStoreSupport
    with AuthAndUncertainEntityTypeJourneyBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[UncertainEntityTypeJourneyStore].toInstance(mockUncertainEntityTypeJourneyStore)
  )

  val controller = instanceOf[EntityTypeController]

  val individualLoginData =
    Fixtures.individualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyLoginData =
    CompanyLoginData(GGCredId(""), None, None, None)

  "EntityTypeController" when {

    "handling requests to the entity type page" must {

      def performAction(): Future[Result] = controller.entityType(FakeRequest())

      behave like authAndSessionDataBehaviour(() => performAction())

      "display the page" when {

        def test(session: HECSession, value: Option[String]) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("entityType.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              testRadioButtonOptions(
                doc,
                List(
                  messageFromMessageKey("entityType.individual"),
                  messageFromMessageKey("entityType.company")
                ),
                List(
                  Some(messageFromMessageKey("entityType.individual.hint")),
                  Some(messageFromMessageKey("entityType.company.hint"))
                )
              )

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.EntityTypeController.entityTypeSubmit.url
            }
          )
        }

        "the user has not previously answered the question" in {
          val session = IndividualHECSession.newSession(individualLoginData)
          test(session, None)

        }

        "the user has previously answered the question" in {
          val session =
            Fixtures.individualHECSession(
              loginData = individualLoginData,
              retrievedJourneyData = IndividualRetrievedJourneyData.empty,
              userAnswers = Fixtures.completeIndividualUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToTwoYears,
                TaxSituation.PAYE,
                Some(YesNoAnswer.Yes),
                Some(EntityType.Individual)
              )
            )
          test(session, Some("0"))
        }

      }

    }

    "handling submits to the entity type page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.entityTypeSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = CompanyHECSession.newSession(companyLoginData)

        def testFormError(data: (String, String)*)(errorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType, session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(data: _*),
            messageFromMessageKey("entityType.title"),
            messageFromMessageKey(errorMessageKey)
          )
        }

        "nothing is submitted" in {
          testFormError()("entityType.error.required")
        }

        "an index is submitted which is too large" in {
          testFormError("entityType" -> Int.MaxValue.toString)("entityType.error.invalid")
        }

        "a value is submitted which is not a number" in {
          testFormError("entityType" -> "xyz")("entityType.error.invalid")
        }

      }

      "throw an exception" when {

        "the call to update and next fails" in {
          val answers        = IndividualUserAnswers.empty
          val updatedAnswers = IndividualUserAnswers.empty.copy(entityType = Some(EntityType.Individual))
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
            mockJourneyServiceUpdateAndNext(routes.EntityTypeController.entityType, session, updatedSession)(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException] {
            await(performAction("entityType" -> "0"))
          }

        }

      }

      "redirect to the next page" when {

        def testRedirect(session: HECSession, updatedSession: HECSession, value: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.EntityTypeController.entityType, session, updatedSession)(
              Right(mockNextCall)
            )
          }

          checkIsRedirect(performAction("entityType" -> value), mockNextCall)
        }

        "valid data is submitted and user is an Individual" when {

          "user has not previously completed answering questions" in {
            val answers        = IndividualUserAnswers.empty
            val updatedAnswers = IndividualUserAnswers.empty.copy(entityType = Some(EntityType.Individual))
            val session        =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            testRedirect(session, updatedSession, "0")
          }

          "user has previously completed answering questions" in {
            val answers        = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.PAYE,
              Some(YesNoAnswer.Yes)
            )
            val updatedAnswers = IncompleteIndividualUserAnswers
              .fromCompleteAnswers(answers)
              .copy(entityType = Some(EntityType.Individual))
            val session        =
              Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                answers
              )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            testRedirect(session, updatedSession, "0")
          }
        }

        "valid data is submitted and user is a Company" when {

          "user has not previously completed answering questions" in {
            val answers        = CompanyUserAnswers.empty
            val updatedAnswers = CompanyUserAnswers.empty.copy(entityType = Some(EntityType.Company))
            val session        =
              Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            testRedirect(session, updatedSession, "1")
          }

          "user has previously completed answering questions" in {
            val answers        = Fixtures.completeCompanyUserAnswers(
              licenceType = LicenceType.OperatorOfPrivateHireVehicles,
              licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
              licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear
            )
            val updatedAnswers = IncompleteCompanyUserAnswers
              .fromCompleteAnswers(answers)
              .copy(entityType = Some(EntityType.Company))
            val session        =
              Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            testRedirect(session, updatedSession, "1")
          }
        }

      }

    }

    "handling requests to see the wrong GG account page" must {

      def performAction(): Future[Result] = controller.wrongGGAccount(FakeRequest())

      behave like authAndSessionDataBehaviour(() => performAction())

      "return an InternalServerError" when {

        "no selected entity type can be found in session" in {
          val session = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction()))

        }

      }

      "display the page" when {

        def testPage(selectedEntityType: EntityType, expectedP1: String): Unit = {
          val session = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(entityType = Some(selectedEntityType))
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.wrongGGAccount, session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("wrongGGAccount.title"),
            { doc =>
              doc.select("#back").attr("href")          shouldBe mockPreviousCall.url
              doc.select("p.govuk-body").first().text() shouldBe expectedP1
            }
          )
        }

        "the selected entity type is individual" in {
          testPage(
            EntityType.Individual,
            messageFromMessageKey(
              "wrongGGAccount.p1",
              messages("wrongGGAccount.company"),
              messages("wrongGGAccount.individual")
            )
          )
        }

        "the selected entity type is company" in {
          testPage(
            EntityType.Company,
            messageFromMessageKey(
              "wrongGGAccount.p1",
              messages("wrongGGAccount.individual"),
              messages("wrongGGAccount.company")
            )
          )
        }

      }

    }

    "handling requests to see the wrong GG entity type page" must {

      def performAction(): Future[Result] = controller.wrongEntityType(FakeRequest())

      behave like authAndUncertainEntityTypeJourneyBehaviour(
        () => performAction(),
        requireDidConfirmUncertainEntityType = false
      )

      "display the page" when {

        def test(
          session: Option[HECSession],
          uncertainEntityTypeJourney: Option[UncertainEntityTypeJourney],
          mockGetPrevious: Option[() => Unit],
          expectedBack: Call
        ) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Right(session))
            uncertainEntityTypeJourney.foreach(mockGetUncertainEntityTypeJourney)
            mockGetPrevious.foreach(_())
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("wrongEntityType.title"),
            doc => doc.select("#back").attr("href") shouldBe expectedBack.url
          )
        }

        "the user has started a session and they did confirm an uncertain entity type" in {
          test(
            Some(IndividualHECSession.newSession(individualLoginData.copy(didConfirmUncertainEntityType = Some(true)))),
            None,
            None,
            routes.ConfirmUncertainEntityTypeController.entityType
          )
        }

        "the user has started a session and they did not confirm an uncertain entity type" in {
          List(
            Some(false),
            None
          ).foreach { didConfirmUncertainEntityType =>
            withClue(s"For didConfirmUncertainEntityType $didConfirmUncertainEntityType: ") {
              val session = IndividualHECSession.newSession(
                individualLoginData.copy(didConfirmUncertainEntityType = didConfirmUncertainEntityType)
              )
              test(
                Some(session),
                None,
                Some(() =>
                  mockJourneyServiceGetPrevious(routes.EntityTypeController.wrongEntityType, session)(mockPreviousCall)
                ),
                mockPreviousCall
              )
            }

          }
        }

        "the user has not started a session and they are in the process of confirming an uncertain entity type" in {
          test(
            None,
            Some(UncertainEntityTypeJourney(GGCredId("id"), None)),
            None,
            routes.ConfirmUncertainEntityTypeController.entityType
          )

        }

      }

    }

  }

}
