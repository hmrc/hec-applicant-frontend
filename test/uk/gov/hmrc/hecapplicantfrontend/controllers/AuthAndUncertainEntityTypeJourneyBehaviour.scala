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

import play.api.mvc.Result
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait AuthAndUncertainEntityTypeJourneyBehaviour {
  this: ControllerSpec with AuthSupport with SessionSupport with UncertainEntityTypeJourneyStoreSupport =>

  def authAndUncertainEntityTypeJourneyBehaviour(
    performAction: () => Future[Result],
    requireDidConfirmUncertainEntityType: Boolean
  ): Unit = {

    "return an error" when {

      "there is ann error getting from the session store" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Left(Error("")))
        }

        a[RuntimeException] shouldBe thrownBy(await(performAction()))

      }

      "there is an error getting from the uncertain entity type journey store" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Right(None))
          mockGetUncertainEntityTypeJourney(Left(Error("")))
        }

        a[RuntimeException] shouldBe thrownBy(await(performAction()))
      }

    }

    "redirect to the start endpoint" when {

      if (requireDidConfirmUncertainEntityType) {
        "session data is found but the user never had to confirm their entity type" in {
          List(
            Some(false),
            None
          ).foreach { didConfirmUncertainEntityType =>
            withClue(s"For $didConfirmUncertainEntityType ") {
              inSequence {
                mockAuthWithNoRetrievals()

                mockGetSession(
                  Fixtures.individualHECSession(
                    loginData =
                      Fixtures.individualLoginData(didConfirmUncertainEntityType = didConfirmUncertainEntityType)
                  )
                )
              }

              checkIsRedirect(performAction(), routes.StartController.start)
            }

          }

        }
      }

      "neither session data nor an uncertain entity type journey is found" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Right(None))
          mockGetUncertainEntityTypeJourney(Right(None))
        }

        checkIsRedirect(performAction(), routes.StartController.start)
      }

    }

  }
}
