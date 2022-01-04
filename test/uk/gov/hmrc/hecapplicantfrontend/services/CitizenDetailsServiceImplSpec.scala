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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import play.api.test.Helpers._
import uk.gov.hmrc.hecapplicantfrontend.connectors.CitizenDetailsConnector
import uk.gov.hmrc.hecapplicantfrontend.models.{CitizenDetails, DateOfBirth, Error, Name}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{NINO, SAUTR}
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import java.time.LocalDate
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class CitizenDetailsServiceImplSpec extends AnyWordSpec with Matchers with MockFactory {

  val mockCitizenDetailsConnector = mock[CitizenDetailsConnector]

  def mockGetCitizenDetails(expectedNINO: NINO)(response: Either[Error, HttpResponse]) =
    (mockCitizenDetailsConnector
      .getCitizenDetails(_: NINO)(_: HeaderCarrier))
      .expects(expectedNINO, *)
      .returning(EitherT.fromEither[Future](response))

  val citizenDetailsService = new CitizenDetailsServiceImpl(mockCitizenDetailsConnector)

  implicit val hc: HeaderCarrier = HeaderCarrier()

  "CitizenDetailsServiceImpl" when {

    "handling requests to get citizen details" must {

      val responseHeaders = Map.empty[String, Seq[String]]

      "return an error" when {

        def testIsError(mockAction: NINO => Unit): Unit = {
          val nino = NINO("nino")

          mockAction(nino)

          await(citizenDetailsService.getCitizenDetails(nino).value) shouldBe a[Left[_, _]]
        }

        "the call to get citizen details fails" in {
          testIsError(mockGetCitizenDetails(_)(Left(Error(""))))
        }

        "the call to get citizen details comes back with a non-OK response" in {
          testIsError(mockGetCitizenDetails(_)(Right(HttpResponse(500, ""))))
        }

        "there is no JSON in the response body" in {
          testIsError(mockGetCitizenDetails(_)(Right(HttpResponse(200, ""))))
        }

        "the JSON cannot be parsed in the response body" in {
          testIsError(
            mockGetCitizenDetails(_)(
              Right(
                HttpResponse(
                  200,
                  Json.parse("""{ "a" : "b" }"""),
                  responseHeaders
                )
              )
            )
          )
        }

        "a full name cannot be found in the response" in {
          val jsonBodies = List(
            """{
               |  "ids" : {}, "dateOfBirth" : "01122013" 
               |}
               |""".stripMargin,
            """{
               |  "ids" : {}, "dateOfBirth" : "01122013", "name" : { } 
               |}
               |""".stripMargin,
            """{
               |  "ids" : {}, "dateOfBirth" : "01122013", "name" : { "current" : {} } 
               |}
               |""".stripMargin,
            """{
               |  "ids" : {}, "dateOfBirth" : "01122013", "name" : { "current" : { "firstName" : "First" } } 
               |}
               |""".stripMargin,
            """{
               |  "ids" : {}, "dateOfBirth" : "01122013", "name" : { "current" : { "lastName" : "Last" } } 
               |}
               |""".stripMargin
          ).map(Json.parse)

          jsonBodies.foreach { json =>
            withClue(s"For json body $json: ") {
              testIsError(mockGetCitizenDetails(_)(Right(HttpResponse(200, json, responseHeaders))))
            }
          }
        }

        "a date of birth cannot be found in the response" in {
          testIsError(nino =>
            mockGetCitizenDetails(nino)(
              Right(
                HttpResponse(
                  200,
                  Json.parse(
                    s"""{
                       |  "ids" : {}, 
                       |   "name" : { 
                       |     "current" : {
                       |        "firstName" : "First",
                       |        "lastName" : "Last"
                       |      } 
                       |    } 
                       |}
                       |""".stripMargin
                  ),
                  responseHeaders
                )
              )
            )
          )
        }

        "the date of birth in the response cannot be parsed" in {
          testIsError(
            mockGetCitizenDetails(_)(
              Right(
                HttpResponse(
                  200,
                  Json.parse(
                    s"""{
                       |  "ids" : {}, 
                       |  "dateOfBirth" : "2013-12-01",
                       |   "name" : { 
                       |     "current" : {
                       |        "firstName" : "First",
                       |        "lastName" : "Last"
                       |      } 
                       |    } 
                       |}
                       |""".stripMargin
                  ),
                  responseHeaders
                )
              )
            )
          )
        }

        "an SAUTR is found which is invalid" in {
          testIsError(
            mockGetCitizenDetails(_)(
              Right(
                HttpResponse(
                  200,
                  Json.parse(
                    s"""{
                       |  "ids" : { "sautr" : "invalid" }, 
                       |  "dateOfBirth" : "01122013",
                       |   "name" : { 
                       |     "current" : {
                       |        "firstName" : "First",
                       |        "lastName" : "Last"
                       |      } 
                       |    } 
                       |}
                       |""".stripMargin
                  ),
                  responseHeaders
                )
              )
            )
          )
        }

      }

      "return the details found" when {

        "the response from citizen details is OK and the json body can be parsed" in {
          val nino           = NINO("nino")
          val name           = Name("First", "Last")
          val dob            = DateOfBirth(LocalDate.of(2013, 12, 1))
          val sautr          = SAUTR("1234567895")
          val citizenDetails = CitizenDetails(name, dob, Some(sautr))

          val json = Json.parse(
            s"""
               |{
               |  "ids" : {
               |    "sautr" : "1234567895"
               |  }, 
               |  "dateOfBirth" : "01122013",
               |   "name" : { 
               |     "current" : {
               |        "firstName" : "First",
               |        "lastName" : "Last"
               |      } 
               |    } 
               |}
               |""".stripMargin
          )

          mockGetCitizenDetails(nino)(Right(HttpResponse(200, json, responseHeaders)))

          val result = citizenDetailsService.getCitizenDetails(nino)
          await(result.value) shouldBe Right(citizenDetails)
        }

      }

    }

  }

}
