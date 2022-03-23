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

import cats.data.EitherT
import cats.instances.either._
import cats.instances.future._
import cats.instances.int._
import cats.instances.list._
import cats.syntax.alternative._
import cats.syntax.eq._
import org.scalamock.handlers.CallHandler2
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.Helpers._
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.models.iv.IvErrorStatus
import uk.gov.hmrc.hecapplicantfrontend.models.iv.IvErrorStatus._
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.IvService
import uk.gov.hmrc.http.HeaderCarrier

import java.util.UUID
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class IvFailureControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  val mockIvService: IvService = mock[IvService]

  override val overrideBindings: List[GuiceableModule] =
    List[GuiceableModule](
      bind[IvService].toInstance(mockIvService),
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore)
    )

  def mockGetFailedJourneyStatus(
    journeyId: UUID
  )(result: Either[Error, IvErrorStatus]): CallHandler2[UUID, HeaderCarrier, EitherT[Future, Error, IvErrorStatus]] =
    (mockIvService
      .getFailedJourneyStatus(_: UUID)(_: HeaderCarrier))
      .expects(journeyId, *)
      .returning(EitherT.fromEither(result))

  lazy val controller: IvFailureController = instanceOf[IvFailureController]

  "IvFailureController" when {

    "handling IV failure callbacks" must {

      def performAction(journeyId: UUID): Future[Result] =
        controller.ivFailure(journeyId)(FakeRequest())

      "return a technical error" when {

        "there is an error getting the failed journey status" in {
          val journeyId = UUID.randomUUID()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetFailedJourneyStatus(journeyId)(Left(Error("")))
          }

          assertThrows[RuntimeException](await(performAction(journeyId)))
        }

      }

      "redirect to the correct place when the failed journey status is retrieved" in {
        val testCases =
          List(
            Incomplete           -> routes.IvFailureController.technicalIssue,
            FailedMatching       -> routes.IvFailureController.failedMatching,
            FailedIV             -> routes.IvFailureController.failedIV,
            InsufficientEvidence -> routes.IvFailureController.insufficientEvidence,
            LockedOut            -> routes.IvFailureController.lockedOut,
            UserAborted          -> routes.IvFailureController.userAborted,
            Timeout              -> routes.IvFailureController.timedOut,
            TechnicalIssue       -> routes.IvFailureController.technicalIssue,
            PreconditionFailed   -> routes.IvFailureController.preconditionFailed,
            Unknown("")          -> routes.IvFailureController.technicalIssue
          )

        val results = testCases.map { case (ivErrorStatus, expectedRedirectTo) =>
          val journeyId = UUID.randomUUID()

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetFailedJourneyStatus(journeyId)(Right(ivErrorStatus))
          }

          val result           = performAction(journeyId)
          val resultStatus     = status(result)
          val resultRedirectTo = redirectLocation(result)

          if (resultStatus =!= SEE_OTHER)
            Left(s"For $ivErrorStatus http status $resultStatus was not equal to $SEE_OTHER")
          else if (!resultRedirectTo.contains(expectedRedirectTo.url))
            Left(s"For $ivErrorStatus redirect location $resultRedirectTo was not equal to ${expectedRedirectTo.url}")
          else
            Right(())
        }

        val errors = results.separate._1
        if (errors.nonEmpty) fail(errors.mkString("\n"))
      }

    }

    "handling requests for the 'failed matching' page" must {

      def performAction(): Future[Result] = controller.failedMatching(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.failedMatching.title"),
          { doc =>
            val button = doc.select(".govuk-button")
            button.attr("href") shouldBe routes.IvFailureController.retry.url
          }
        )

      }

    }

    "handling requests to the 'failed IV' page" must {

      def performAction(): Future[Result] = controller.failedIV(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.failedIv.title"),
          { doc =>
            val button = doc.select(".govuk-button")
            button.attr("href") shouldBe routes.IvFailureController.retry.url
          }
        )

      }
    }

    "handling requests to the 'insufficient evidence' page" must {

      def performAction(): Future[Result] = controller.insufficientEvidence(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.insufficientEvidence.title")
        )
      }

    }

    "handling requests to the 'locked out' page" must {

      def performAction(): Future[Result] = controller.lockedOut(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.lockedOut.title")
        )
      }

    }

    "handling requests to the 'user aborted' page" must {

      def performAction(): Future[Result] = controller.userAborted(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.userAborted.title"),
          { doc =>
            val button = doc.select(".govuk-button")
            button.attr("href") shouldBe routes.IvFailureController.retry.url
          }
        )
      }

    }

    "handling requests to the 'timed out' page" must {

      def performAction(): Future[Result] = controller.timedOut(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.timeout.title"),
          { doc =>
            val button = doc.select(".govuk-button")
            button.attr("href") shouldBe routes.IvFailureController.retry.url
          }
        )
      }

    }

    "handling requests to the 'technical issue' page" must {

      def performAction(): Future[Result] = controller.technicalIssue(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.technicalIssue.title"),
          { doc =>
            val button = doc.select(".govuk-button")
            button.attr("href") shouldBe routes.IvFailureController.retry.url
          }
        )
      }

    }

    "handling requests to the 'precondition failed' page" must {

      def performAction(): Future[Result] = controller.preconditionFailed(FakeRequest())

      behave like authBehaviour(performAction)

      "display the page" in {
        mockAuthWithNoRetrievals()

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("iv.preconditionFailed.title")
        )
      }

    }

    "handling requests to retry" must {

      def performAction(): Future[Result] = controller.retry(FakeRequest())

      behave like authBehaviour(performAction)

      "redirect to the start endpoint" in {
        mockAuthWithNoRetrievals()

        checkIsRedirect(performAction(), routes.StartController.start)
      }

    }

  }

}
