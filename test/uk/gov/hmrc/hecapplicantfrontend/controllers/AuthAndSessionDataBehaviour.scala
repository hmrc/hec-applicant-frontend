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

import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.mvc.Result
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.EmptyRetrieval
import uk.gov.hmrc.auth.core.{AuthorisationException, BearerTokenExpired, IncorrectCredentialStrength, InsufficientEnrolments, InternalError, InvalidBearerToken, MissingBearerToken, NoActiveSession, SessionRecordNotFound, UnsupportedAffinityGroup, UnsupportedAuthProvider, UnsupportedCredentialRole}
import uk.gov.hmrc.hecapplicantfrontend.models.Error

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait AuthAndSessionDataBehaviour { this: ControllerSpec with AuthSupport with SessionSupport =>

  val selfUrl = "http://self:123"

  val basGatewayUrl = "https://bas:456"

  val ggOrigin = "ggOrigin"

  override lazy val additionalConfig: Configuration = Configuration(
    ConfigFactory.parseString(
      s"""
         | self.url = "$selfUrl"
         | auth {
         |   bas-gateway.url = "$basGatewayUrl"
         |   gg.origin = "$ggOrigin"
         | }
         |""".stripMargin
    )
  )

  def authBehaviour(performAction: () => Future[Result]): Unit = {
    "redirect to the login page when the user is not logged in" in {
      List[NoActiveSession](
        BearerTokenExpired(),
        MissingBearerToken(),
        InvalidBearerToken(),
        SessionRecordNotFound()
      ).foreach { e =>
        withClue(s"For AuhtorisationException $e: ") {
          mockAuth(EmptyPredicate, EmptyRetrieval)(Future.failed(e))

          val result = performAction()
          checkIsRedirect(result, s"$basGatewayUrl?continue=$selfUrl/hec-applicant-frontend/start&origin=$ggOrigin")
        }
      }

    }

    "return an InternalServerError when an AuthorisationException is thrown" in {
      List[AuthorisationException](
        InsufficientEnrolments(),
        UnsupportedAffinityGroup(),
        UnsupportedCredentialRole(),
        UnsupportedAuthProvider(),
        IncorrectCredentialStrength(),
        InternalError()
      ).foreach { e =>
        withClue(s"For error $e: ") {
          mockAuth(EmptyPredicate, EmptyRetrieval)(Future.failed(e))

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }
      }
    }
  }

  def authAndSessionDataBehaviour(performAction: () => Future[Result]): Unit = {

    authBehaviour(performAction)

    "return an InternalServerError when there is an error getting session data" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(Left(Error("")))
      }

      status(performAction()) shouldBe INTERNAL_SERVER_ERROR
    }

    "redirect to the start endpoint when there is no session data" in {
      inSequence {
        mockAuthWithNoRetrievals()
        mockGetSession(Right(None))
      }

      checkIsRedirect(performAction(), routes.StartController.start())
    }

  }

}
