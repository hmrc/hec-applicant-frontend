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

package uk.gov.hmrc.hecapplicantfrontend.testonly.models

import play.api.libs.json.{Format, JsObject, JsResult, JsValue, Json}

import java.util.Locale
import scala.reflect.ClassTag

object EitherUtils {

  implicit def eitherFormat[A, B](implicit
    aFormat: Format[A],
    bFormat: Format[B],
    aTag: ClassTag[A],
    bTag: ClassTag[B]
  ): Format[Either[A, B]] =
    new Format[Either[A, B]] {
      val leftFieldName  = aTag.runtimeClass.getSimpleName.toLowerCase(Locale.UK)
      val rightFieldName = bTag.runtimeClass.getSimpleName.toLowerCase(Locale.UK)

      override def reads(json: JsValue): JsResult[Either[A, B]] =
        (json \ leftFieldName)
          .validate[A]
          .map[Either[A, B]](Left(_))
          .orElse((json \ rightFieldName).validate[B].map(Right(_)))

      override def writes(o: Either[A, B]): JsValue =
        o.fold(
          a ⇒ JsObject(Seq(leftFieldName → Json.toJson(a))),
          b ⇒ JsObject(Seq(rightFieldName → Json.toJson(b)))
        )
    }

}
