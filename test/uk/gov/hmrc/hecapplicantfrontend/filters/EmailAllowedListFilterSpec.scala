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

package uk.gov.hmrc.hecapplicantfrontend.filters

import akka.actor.ActorSystem
import akka.stream.Materializer
import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.Configuration
import play.api.mvc.{RequestHeader, Result, Results}
import play.api.test.FakeRequest
import uk.gov.hmrc.auth.core.AuthConnector
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class EmailAllowedListFilterSpec extends AnyWordSpec with Matchers with MockFactory with BeforeAndAfterAll {

  implicit val system            = ActorSystem()
  implicit val mat: Materializer = Materializer(system)

  def config(isEnabled: Boolean): Configuration = Configuration(
    ConfigFactory.parseString(
      s"""
        | user-allow-list = ["user@test.com"]
        | userAllowedList.enabled = $isEnabled
        | 
        | """.stripMargin
    )
  )

  override def afterAll(): Unit = {
    super.afterAll()
    mat.shutdown()
  }

  val mockAuthConnector = mock[AuthConnector]
//  def overrideBindings: List[GuiceableModule] = List(
//    bind[AuthConnector].toInstance(mockAuthConnector)
//  )

  val emailAllowedListFilter = new EmailAllowedListFilter(mat, mockAuthConnector, config(true))

  "EmailAllowedListFilterSpec" when {

    "enable config is false, moe to next page" when {

      "user allowed email list contains the email in enrollment" in {

        val individualLoginData = Fixtures.individualLoginData(emailAddress = Some(EmailAddress("user@test.com")))
        val hecSession          = Fixtures.individualHECSession(individualLoginData)

        val requestHeader: RequestHeader => Future[Result] = _ => Future.successful(Results.Ok)
        val request                                        = FakeRequest("GET", "/some-url").withSession(hecSession)
        val result                                         = emailAllowedListFilter(requestHeader)(request)
        await(result) shouldBe 200

      }
    }

  }

}
