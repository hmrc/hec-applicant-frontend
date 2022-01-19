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

package uk.gov.hmrc.hecapplicantfrontend.testonly.services

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import play.api.libs.ws.{DefaultWSCookie, WSCookie, WSResponse}
import play.api.test.Helpers._
import play.api.mvc.{Cookie, Session, SessionCookieBaker}
import uk.gov.hmrc.auth.core.AffinityGroup.Individual
import uk.gov.hmrc.auth.core.ConfidenceLevel.L250
import uk.gov.hmrc.crypto.{Crypted, Decrypter, Encrypter, PlainBytes, PlainContent, PlainText}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.GGCredId
import uk.gov.hmrc.hecapplicantfrontend.models.Error
import uk.gov.hmrc.hecapplicantfrontend.testonly.connectors.AuthLoginStubConnector
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.LoginData
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.filters.crypto.SessionCookieCrypto

import scala.concurrent.ExecutionContext.Implicits.global

class AuthLoginStubServiceImplSpec extends Matchers with AnyWordSpecLike with MockFactory {

  val mockConnector = mock[AuthLoginStubConnector]

  val mockDecrypter = mock[Decrypter]

  val mockEncrypter = mock[Encrypter]

  val mockCrypto = new Encrypter with Decrypter {
    override def encrypt(plain: PlainContent): Crypted =
      mockEncrypter.encrypt(plain)

    override def decrypt(reversiblyEncrypted: Crypted): PlainText =
      mockDecrypter.decrypt(reversiblyEncrypted)

    override def decryptAsBytes(reversiblyEncrypted: Crypted): PlainBytes =
      mockDecrypter.decryptAsBytes(reversiblyEncrypted)
  }

  val mockSessionCookieCrypto = SessionCookieCrypto(mockCrypto)

  val mockSessionCookieBaker = mock[SessionCookieBaker]

  val mockWSResponse = mock[WSResponse]

  def mockLogin(loginData: LoginData)(result: Either[Error, WSResponse]) =
    (mockConnector
      .login(_: LoginData)(_: HeaderCarrier))
      .expects(loginData, *)
      .returning(EitherT.fromEither(result))

  def mockDecrypt(crypted: Crypted)(result: PlainText) =
    (mockDecrypter
      .decrypt(_: Crypted))
      .expects(crypted)
      .returning(result)

  def mockDecodeFromCookie(cookie: Cookie)(result: Session) =
    (mockSessionCookieBaker
      .decodeFromCookie(_: Option[Cookie]))
      .expects(Some(cookie))
      .returning(result)

  def mockWsResponseStatus(status: Int) =
    (mockWSResponse.status _).expects().returning(status)

  def mockWsResponseBody(result: String) =
    (mockWSResponse.body _).expects().returning(result)

  def mockWsResponseHeader(headerName: String)(result: Option[String]) =
    (mockWSResponse.header _).expects(headerName).returning(result)

  def mockWsResponseCookie(cookieName: String)(result: Option[WSCookie]) =
    (mockWSResponse.cookie _).expects(cookieName).returning(result)

  val service = new AuthLoginStubServiceImpl(mockConnector, mockSessionCookieCrypto, mockSessionCookieBaker)

  "AuthLoginStubServiceImpl" when {

    "handling requests to login" must {

      implicit val hc: HeaderCarrier = HeaderCarrier()

      val loginData =
        LoginData(
          GGCredId(""),
          "redirect",
          L250,
          Individual,
          None,
          None,
          None,
          List.empty
        )

      "return an error" when {

        "the http call fails" in {
          mockLogin(loginData)(Left(Error("")))

          await(service.login(loginData).value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with a status which is not 303 (SEE OTHER)" in {
          inSequence {
            mockLogin(loginData)(Right(mockWSResponse))
            mockWsResponseStatus(OK)
            mockWsResponseBody("body")
          }

          await(service.login(loginData).value) shouldBe a[Left[_, _]]
        }

        "the http response comes back with status 303 (SEE OTHER) but" when {

          "a location header cannot be found" in {
            inSequence {
              mockLogin(loginData)(Right(mockWSResponse))
              mockWsResponseStatus(SEE_OTHER)
              mockWsResponseHeader("location")(None)
            }

            await(service.login(loginData).value) shouldBe a[Left[_, _]]
          }

          "the redirect location does not match the one in the login data request" in {
            inSequence {
              mockLogin(loginData)(Right(mockWSResponse))
              mockWsResponseStatus(SEE_OTHER)
              mockWsResponseHeader("location")(Some(s"${loginData.redirectUrl}/other"))
            }

            await(service.login(loginData).value) shouldBe a[Left[_, _]]
          }
        }

        "there is no 'mdtp' cookie in the response" in {
          inSequence {
            mockLogin(loginData)(Right(mockWSResponse))
            mockWsResponseStatus(SEE_OTHER)
            mockWsResponseHeader("location")(Some(loginData.redirectUrl))
            mockWsResponseCookie("mdtp")(None)
          }

          await(service.login(loginData).value) shouldBe a[Left[_, _]]

        }

      }

      "return successfully" when {

        "the login is successful and the session can be extracted from the mdtp cookie in the response" in {
          val session = Session(Map("key" -> "value"))

          inSequence {
            mockLogin(loginData)(Right(mockWSResponse))
            mockWsResponseStatus(SEE_OTHER)
            mockWsResponseHeader("location")(Some(loginData.redirectUrl))
            mockWsResponseCookie("mdtp")(Some(DefaultWSCookie("mdtp", "cookieValue")))
            mockDecrypt(Crypted("cookieValue"))(PlainText("decryptedCookieValue"))
            mockDecodeFromCookie(Cookie("mdtp", "decryptedCookieValue"))(session)
          }

          await(service.login(loginData).value) shouldBe Right(session)
        }

      }

    }

  }

}
