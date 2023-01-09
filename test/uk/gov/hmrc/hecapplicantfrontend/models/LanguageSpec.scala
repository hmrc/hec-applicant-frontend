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

package uk.gov.hmrc.hecapplicantfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.{DefaultLangs, DefaultMessagesApi, Lang}
import play.api.libs.json.{JsError, JsNumber, JsString, JsSuccess, Json}
import play.api.mvc.{Cookie, MessagesRequest}
import play.api.test.FakeRequest

class LanguageSpec extends AnyWordSpec with Matchers {

  "Language" when {

    def forEachLanguage[A](f: Language => A) =
      List(Language.English, Language.Welsh).foreach { lang =>
        withClue(s"For language $lang: ") {
          f(lang)
        }
      }

    "handling calls to fromRequest" must {

      val unknownLanguageCode = "ZZ"

      val messagesApi =
        new DefaultMessagesApi(
          langs = new DefaultLangs(
            Seq(
              Lang(Language.English.code),
              Lang(Language.Welsh.code),
              Lang(unknownLanguageCode)
            )
          )
        )

      def messagesRequest(languageCode: String) =
        new MessagesRequest(
          FakeRequest().withCookies(Cookie("PLAY_LANG", languageCode)),
          messagesApi
        )

      "return an error if the language is not recognised" in {
        Language.fromRequest(messagesRequest(unknownLanguageCode)) shouldBe a[Left[_, _]]
      }

      "return the correct language" in {
        forEachLanguage { lang =>
          Language.fromRequest(messagesRequest(lang.code)) shouldBe Right(lang)
        }
      }

    }

    "handling json writes" must {

      "write a json string using the language code" in {
        forEachLanguage { lang =>
          Json.toJson(lang) shouldBe JsString(lang.toString)
        }

      }

    }

    "handling json reads" must {

      "return an error" when {

        "the value is not a string" in {
          JsNumber(1).validate[Language] shouldBe a[JsError]
        }

        "the string value is not a recognised language code" in {
          JsString("XX").validate[Language] shouldBe a[JsError]
        }

      }

      "return the correct language" when {

        "a string value is recognised" in {
          forEachLanguage { lang =>
            JsString(lang.toString).validate[Language] shouldBe JsSuccess(lang)
          }

        }

      }

    }

  }

}
