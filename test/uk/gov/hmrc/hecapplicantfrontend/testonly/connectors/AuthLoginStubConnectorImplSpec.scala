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

package uk.gov.hmrc.hecapplicantfrontend.testonly.connectors

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.Configuration
import play.api.libs.ws.{BodyWritable, WSClient, WSRequest, WSResponse}
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.hecapplicantfrontend.connectors.ConnectorSpec
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Error}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.LoginData
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class AuthLoginStubConnectorImplSpec extends ConnectorSpec with Matchers with AnyWordSpecLike with MockFactory {

  val authLoginStubBaseUrl = "baseUrl"

  val config = Configuration(
    ConfigFactory.parseString(
      s"""auth-login-stub.base-url = "$authLoginStubBaseUrl" """
    )
  )

  val mockWsClient = mock[WSClient]

  val mockWsRequest = mock[WSRequest]

  val connector = new AuthLoginStubConnectorImpl(config, mockWsClient)

  def mockPost(url: String, withFollowRedirects: Boolean, headers: List[(String, String)], body: String)(
    response: Future[WSResponse]
  ) =
    inSequence {
      (mockWsClient.url _)
        .expects(url)
        .returning(mockWsRequest)
      (mockWsRequest.withFollowRedirects _)
        .expects(withFollowRedirects)
        .returning(mockWsRequest)
      (mockWsRequest.withHttpHeaders _)
        .expects(headers)
        .returning(mockWsRequest)
      (mockWsRequest
        .post[String](_: String)(_: BodyWritable[String]))
        .expects(
          where { case (actualBody: String, _) =>
            val expectedFormKeyValues = body.split("&").toSet
            val actualFormKeyValues   = actualBody.split("&").toSet
            val (unexpected, missing) =
              actualFormKeyValues.diff(expectedFormKeyValues) -> expectedFormKeyValues.diff(actualFormKeyValues)

            if (missing.nonEmpty || unexpected.nonEmpty)
              fail(
                s"Found unexpected form key values $unexpected and did not find expected form key values $missing"
              )

            true
          }
        )
        .returning(response)
    }

  "AuthLoginStubConnectorImpl" when {

    "handling requests to login" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val expectedUrl = s"$authLoginStubBaseUrl/auth-login-stub/gg-sign-in"

      val expectedFollowRedirect = false

      val expectedHeaders = List("Content-Type" -> "application/x-www-form-urlencoded")

      "return an error" when {

        "the http call fails" in {
          val fullyPopulatedLoginData = LoginData(
            GGCredId("credId"),
            "redirect/url",
            ConfidenceLevel.L250,
            AffinityGroup.Individual,
            EmailAddress("email@test.com"),
            Some(NINO("nino")),
            Some(Enrolment("enrolmentKey", List(EnrolmentIdentifier("idKey", "idValue")), "state")),
            List.empty
          )

          val expectedBody =
            "authorityId=credId&redirectionUrl=redirect%2Furl&credentialStrength=strong&" +
              "confidenceLevel=250&affinityGroup=Individual&credentialRole=User&" +
              "email=email%40test.com&nino=nino&" +
              "enrolment%5B0%5D.name=enrolmentKey&" +
              "enrolment%5B0%5D.taxIdentifier%5B0%5D.name=idKey&" +
              "enrolment%5B0%5D.taxIdentifier%5B0%5D.value=idValue&" +
              "enrolment%5B0%5D.state=state"

          val exception = new Exception("Oh no!")

          mockPost(
            expectedUrl,
            expectedFollowRedirect,
            expectedHeaders,
            expectedBody
          )(Future.failed(exception))

          await(connector.login(fullyPopulatedLoginData).value) shouldBe Left(Error(exception))
        }

      }

      "return a response" when {

        "the http call succeeds" in {
          val loginData = LoginData(
            GGCredId("credId"),
            "redirect/url",
            ConfidenceLevel.L50,
            AffinityGroup.Organisation,
            EmailAddress("email@test.com"),
            None,
            None,
            List.empty
          )

          val expectedBody =
            "authorityId=credId&redirectionUrl=redirect%2Furl&credentialStrength=strong&" +
              "confidenceLevel=50&affinityGroup=Organisation&credentialRole=User&" +
              "email=email%40test.com"

          val response = mock[WSResponse]

          mockPost(
            expectedUrl,
            expectedFollowRedirect,
            expectedHeaders,
            expectedBody
          )(Future.successful(response))

          await(connector.login(loginData).value) shouldBe Right(response)
        }

      }

    }
  }

}
