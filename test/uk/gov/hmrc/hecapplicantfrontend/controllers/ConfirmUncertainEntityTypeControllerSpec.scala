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
import cats.instances.future._
import play.api.inject.bind
import play.api.mvc.{Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.ids.GGCredId
import uk.gov.hmrc.hecapplicantfrontend.models.{EntityType, Error, UncertainEntityTypeJourney}
import uk.gov.hmrc.hecapplicantfrontend.repos.{SessionStore, UncertainEntityTypeJourneyStore}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class ConfirmUncertainEntityTypeControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with JourneyServiceSupport
    with UncertainEntityTypeJourneyStoreSupport
    with AuthAndUncertainEntityTypeJourneyBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[UncertainEntityTypeJourneyStore].toInstance(mockUncertainEntityTypeJourneyStore)
  )

  val controller = instanceOf[ConfirmUncertainEntityTypeController]

  def mockDeleteSession(result: Either[Error, Unit]) =
    (mockSessionStore
      .delete()(_: Request[_]))
      .expects(*)
      .returning(EitherT.fromEither(result))

  val ggCredId: GGCredId = GGCredId("cred")

  val companySession = Fixtures.companyHECSession(
    Fixtures.companyLoginData(ggCredId = ggCredId, didConfirmUncertainEntityType = Some(true))
  )

  val individualSession = Fixtures.individualHECSession(
    Fixtures.individualLoginData(ggCredId = ggCredId, didConfirmUncertainEntityType = Some(true))
  )

  "ConfirmUncertainEntityTypeController" when {

    "handling requests to display the confirm uncertain entity type page" must {

      def performAction(): Future[Result] = controller.entityType(FakeRequest())

      behave like authAndUncertainEntityTypeJourneyBehaviour(performAction, requireDidConfirmUncertainEntityType = true)

      "display the page" when {

        def testPage(
          expectedSelectedOption: Option[EntityType],
          expectedBackUrl: String
        ) =
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("entityType.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe expectedBackUrl

              doc
                .select("form")
                .attr("action") shouldBe routes.ConfirmUncertainEntityTypeController.entityTypeSubmit.url

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

              val individualOption = doc.select("#entityType")
              val companyOption    = doc.select("#entityType-2")

              individualOption.hasAttr("checked") shouldBe expectedSelectedOption.contains(EntityType.Individual)
              companyOption.hasAttr("checked")    shouldBe expectedSelectedOption.contains(EntityType.Company)
            }
          )

        "the user has not answered the question before" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Right(None))
            mockGetUncertainEntityTypeJourney(UncertainEntityTypeJourney(ggCredId, None))
          }

          testPage(None, appConfig.applicantServiceGuidanceUrl)
        }

        "there is an active session with incomplete answers and the user previously confirmed their entity type" in {
          List(
            EntityType.Company    -> companySession,
            EntityType.Individual -> individualSession
          ).foreach { case (expectedOption, session) =>
            withClue(s"For entity type $expectedOption: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Right(Some(session)))
              }

              testPage(Some(expectedOption), appConfig.applicantServiceGuidanceUrl)
            }

          }
        }

        "there is an active session with complete answers and the user previously confirmed their entity type" in {
          List(
            EntityType.Company    -> companySession.copy(userAnswers = Fixtures.completeCompanyUserAnswers()),
            EntityType.Individual -> individualSession.copy(userAnswers = Fixtures.completeIndividualUserAnswers())
          ).foreach { case (expectedOption, session) =>
            withClue(s"For entity type $expectedOption: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(Right(Some(session)))
              }

              testPage(Some(expectedOption), routes.CheckYourAnswersController.checkYourAnswers.url)
            }

          }
        }

        "there is no active session but there is an UncertainEntityTypeJourney " +
          "with an entity type already there" in {
            List(
              EntityType.Company,
              EntityType.Individual
            ).foreach { case expectedOption =>
              withClue(s"For entity type $expectedOption: ") {
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(Right(None))
                  mockGetUncertainEntityTypeJourney(
                    UncertainEntityTypeJourney(ggCredId, Some(expectedOption))
                  )
                }

                testPage(Some(expectedOption), appConfig.applicantServiceGuidanceUrl)
              }
            }

          }

      }

    }

    "handling submits to the confirm uncertain entity type page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.entityTypeSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndUncertainEntityTypeJourneyBehaviour(
        () => performAction(),
        requireDidConfirmUncertainEntityType = true
      )

      "show a form error" when {

        def testFormError(data: (String, String)*)(errorMessageKey: String) = {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Right(None))
            mockGetUncertainEntityTypeJourney(UncertainEntityTypeJourney(ggCredId, None))
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

      "return a technical error" when {

        "there is an error deleting a session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualSession)
            mockDeleteSession(Left(Error("")))
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction("entityType" -> "1")))
        }

        "there is an error updating the uncertain entity type journey store" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Right(None))
            mockGetUncertainEntityTypeJourney(UncertainEntityTypeJourney(ggCredId, None))
            mockUpdateUncertainEntityTypeJourney(
              UncertainEntityTypeJourney(ggCredId, Some(EntityType.Company))
            )(
              Left(Error(""))
            )
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction("entityType" -> "1")))
        }

      }

      "redirect to the next page without performing an update when the answer has not changed and" when {

        "the user had already started a session" when {

          "the session does not contain complete answers" in {
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(individualSession)
              mockFirstPge(individualSession)(mockNextCall)

            }

            checkIsRedirect(performAction("entityType" -> "0"), mockNextCall)
          }

          "the session contains complete answers" in {
            val session = individualSession.copy(userAnswers = Fixtures.completeIndividualUserAnswers())
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            checkIsRedirect(performAction("entityType" -> "0"), routes.CheckYourAnswersController.checkYourAnswers)
          }
        }

        "the user had not already started a session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Right(None))
            mockGetUncertainEntityTypeJourney(
              UncertainEntityTypeJourney(ggCredId, Some(EntityType.Company))
            )
          }

          checkIsRedirect(performAction("entityType" -> "1"), routes.StartController.start)
        }

      }

      "perform an update and redirect to the next page when a new answer is given and" when {

        "the user had already started a session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(individualSession)
            mockDeleteSession(Right(()))
            mockUpdateUncertainEntityTypeJourney(UncertainEntityTypeJourney(ggCredId, Some(EntityType.Company)))(
              Right(())
            )
          }

          checkIsRedirect(performAction("entityType" -> "1"), routes.StartController.start)
        }

        "the user had not already started a session" in {
          val journey = UncertainEntityTypeJourney(ggCredId, None)
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Right(None))
            mockGetUncertainEntityTypeJourney(journey)
            mockUpdateUncertainEntityTypeJourney(journey.copy(userSuppliedEntityType = Some(EntityType.Individual)))(
              Right(())
            )
          }

          checkIsRedirect(performAction("entityType" -> "0"), routes.StartController.start)
        }

      }

    }
  }

}
