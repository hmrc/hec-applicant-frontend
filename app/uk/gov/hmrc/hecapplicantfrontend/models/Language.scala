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

import play.api.i18n.Lang
import play.api.libs.json.{Format, JsError, JsString, JsSuccess, Reads, Writes}
import play.api.mvc.MessagesRequest

import java.util.Locale

sealed trait Language extends Product with Serializable {
  val code: String
}

object Language {

  case object English extends Language {
    val code = "en"
  }

  case object Welsh extends Language {
    val code = "cy"
  }

  implicit def toPlayLang(l: Language): Lang = Lang(l.code)

  def fromRequest(request: MessagesRequest[_]): Either[String, Language] =
    request.messages.lang.code.toLowerCase(Locale.UK) match {
      case English.code => Right(English)
      case Welsh.code   => Right(Welsh)
      case other        => Left(s"Found unsupported language code $other")
    }

  implicit val format: Format[Language] = Format(
    Reads {
      case JsString(s) =>
        s match {
          case "English" => JsSuccess(English)
          case "Welsh"   => JsSuccess(Welsh)
          case other     => JsError(s"Found unsupported language $other")
        }

      case other => JsError(s"Expected string but got ${other.getClass.getSimpleName}")
    },
    Writes(l => JsString(l.toString))
  )

}
