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

import java.time.LocalDate

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EntityTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
  )

  val controller = instanceOf[EntityTypeController]

  val individuaRetrievedlData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)

  val companyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None)

  "EntityTypeController" when {

    "handling requests to the entity type page" must {

      def performAction(): Future[Result] = controller.entityType(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" when {

        "the user has not previously answered the question" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("entityType.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.EntityTypeController.entityTypeSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {
          val session =
            HECSession(
              individuaRetrievedlData,
              CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToTwoYears,
                TaxSituation.PAYE,
                Some(IncomeDeclared.Yes),
                Some(EntityType.Individual)
              ),
              None
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("entityType.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "0"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.EntityTypeController.entityTypeSubmit().url
            }
          )
        }

      }

    }

    "handling submits to the entity type page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.entityTypeSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = HECSession(companyRetrievedData, UserAnswers.empty, None)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("entityType.title"),
            messageFromMessageKey("entityType.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("entityType" -> Int.MaxValue.toString),
            messageFromMessageKey("entityType.title"),
            messageFromMessageKey("entityType.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.entityType(), session)(mockPreviousCall)
          }

          checkFormErrorIsDisplayed(
            performAction("entityType" -> "xyz"),
            messageFromMessageKey("entityType.title"),
            messageFromMessageKey("entityType.error.invalid")
          )
        }

      }

      "return an internal server error" when {

        "the call to update and next fails" in {
          val answers        = UserAnswers.empty
          val updatedAnswers = UserAnswers.empty.copy(entityType = Some(EntityType.Individual))
          val session        = HECSession(individuaRetrievedlData, answers, None)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(routes.EntityTypeController.entityType(), session, updatedSession)(
              Left(Error(new Exception))
            )
          }

          status(performAction("entityType" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user is Individual has not previously completed answering questions" in {
            val answers        = UserAnswers.empty
            val updatedAnswers = UserAnswers.empty.copy(entityType = Some(EntityType.Company))
            val session        = HECSession(individuaRetrievedlData, answers, None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.EntityTypeController.entityType(), session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("entityType" -> "1"), mockNextCall)
          }

          "the user is Company has not previously completed answering questions" in {
            val answers        = UserAnswers.empty
            val updatedAnswers = UserAnswers.empty.copy(entityType = Some(EntityType.Company))
            val session        = HECSession(companyRetrievedData, answers, None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.EntityTypeController.entityType(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("entityType" -> "1"), mockNextCall)
          }

          "the user has previously completed answering questions" in {
            val answers        = CompleteUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.PAYE,
              Some(IncomeDeclared.Yes),
              None
            )
            val updatedAnswers = IncompleteUserAnswers
              .fromCompleteAnswers(answers)
              .copy(entityType = Some(EntityType.Company))
            val session        = HECSession(individuaRetrievedlData, answers, None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(routes.EntityTypeController.entityType(), session, updatedSession)(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("entityType" -> "1"), mockNextCall)
          }

          "the user is a company and  has previously completed answering questions" in {
            val answers        = CompleteUserAnswers(
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToOneYear,
              TaxSituation.SAPAYE,
              Some(IncomeDeclared.Yes),
              None
            )
            val updatedAnswers = IncompleteUserAnswers
              .fromCompleteAnswers(answers)
              .copy(entityType = Some(EntityType.Company))
            val session        = HECSession(companyRetrievedData, answers, None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.EntityTypeController.entityType(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("entityType" -> "1"), mockNextCall)
          }
        }

      }

    }

    "handling requests to see the wrong GG account page" must {

      def performAction(): Future[Result] = controller.wrongGGAccount(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "return an InternalServerError" when {

        "no selected entity type can be found in session" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR

        }

      }

      "display the page" when {

        "a selected entity type can be found in session" in {
          val session = HECSession(
            individuaRetrievedlData,
            UserAnswers.empty.copy(entityType = Some(EntityType.Company)),
            None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.EntityTypeController.wrongGGAccount(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("wrongGGAccount.title"),
            doc => doc.select("#back").attr("href") shouldBe mockPreviousCall.url
          )

        }

      }

    }

    "handling requests to see the wrong GG entity type page" must {

      def performAction(): Future[Result] = controller.wrongEntityType(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {

        val session = HECSession(
          individuaRetrievedlData,
          UserAnswers.empty,
          None
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.EntityTypeController.wrongEntityType(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("wrongEntityType.title"),
          doc => doc.select("#back").attr("href") shouldBe mockPreviousCall.url
        )

      }

    }

  }

}
