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

package uk.gov.hmrc.hecapplicantfrontend.filters

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import org.scalatest.BeforeAndAfterAll
import play.api.Configuration
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Request, RequestHeader, Result, Results}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.hecapplicantfrontend.controllers.{AuthSupport, ControllerSpec, JourneyServiceSupport, routes}
import uk.gov.hmrc.hecapplicantfrontend.models.EmailAddress
import uk.gov.hmrc.hecapplicantfrontend.models.ids.GGCredId

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailAllowedListFilterSpec
    extends ControllerSpec
    with AuthSupport
    with BeforeAndAfterAll
    with JourneyServiceSupport {

  implicit val system            = ActorSystem()
  implicit val mat: Materializer = Materializer(system)

  def additionalConfig(isEnabled: Boolean): Configuration = Configuration(
    ConfigFactory.parseString(
      s"""
        | email-allow-list {
        | enabled = $isEnabled
        | list = ["user@test.com"]
        | }
        | """.stripMargin
    )
  )

  override def afterAll(): Unit = {
    super.afterAll()
    await(system.terminate())
  }

  override def overrideBindings: List[GuiceableModule] = List(
    bind[AuthConnector].toInstance(mockAuthConnector)
  )

  val retrievals = Retrievals.email

  def mockAuthWithRetrievals(
    retrievedEmailAddress: Option[EmailAddress]
  ) =
    mockAuth(EmptyPredicate, retrievals)(
      Future.successful(retrievedEmailAddress.map(_.value))
    )

  def emailAllowedListFilter(isEnabled: Boolean) =
    new EmailAllowedListFilter(mat, mockAuthConnector, additionalConfig(isEnabled))

  def retrievedGGCredential(ggCredId: GGCredId) =
    Credentials(ggCredId.value, "GovernmentGateway")

  val requestHandler: RequestHeader => Future[Result] = _ => Future.successful(Results.Ok)

  "EmailAllowedListFilterSpec" when {

    "email allowed config is false, move to next page, irrespective of enrollment contains the email id or not" in {

      val request = FakeRequest()
      val result  = emailAllowedListFilter(false)(requestHandler)(request)
      status(result) shouldBe 200
    }

    "email allowed config is true" must {

      "move to the next page" when {

        def test(request: Request[_], emailAddress: EmailAddress, authExpected: Boolean) = {
          if (authExpected) mockAuthWithRetrievals(Some(emailAddress))

          val result = emailAllowedListFilter(true)(requestHandler)(request)
          status(result) shouldBe 200
        }

        "user allowed email list contains the email in enrollment" in {
          test(FakeRequest(), EmailAddress("user@test.com"), authExpected = true)
        }

        "user allowed email list contains the email with different case in enrollment" in {
          test(FakeRequest(), EmailAddress("UsEr@teSt.cOm"), authExpected = true)
        }

        "the user is not on the allowed email list and" when afterWord("the request") {

          "is for the access denied page" in {
            test(
              FakeRequest(routes.AccessDeniedController.accessDenied),
              EmailAddress("user1@test.com"),
              authExpected = false
            )
          }

          "uri contains 'hmrc-frontend'" in {
            test(
              FakeRequest("GET", "http://host/hmrc-frontend/x"),
              EmailAddress("user1@test.com"),
              authExpected = false
            )
          }

          "uri contains 'assets'" in {
            test(FakeRequest("GET", "http://host/assets/x"), EmailAddress("user1@test.com"), authExpected = false)
          }

          "uri contains 'ping/ping'" in {
            test(FakeRequest("GET", "http://host/ping/ping"), EmailAddress("user1@test.com"), authExpected = false)
          }

        }

      }

      "move to access denied" when {

        "user allowed email list doesn't contain the email in enrollment" in {

          val request = FakeRequest()
          mockAuthWithRetrievals(
            Some(EmailAddress("user1@test.com"))
          )
          val result  = emailAllowedListFilter(true)(requestHandler)(request)
          checkIsRedirect(result, routes.AccessDeniedController.accessDenied)
        }

      }

    }

  }
}
