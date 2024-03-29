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

package uk.gov.hmrc.hecapplicantfrontend.util

import cats.Eq
import cats.instances.int._
import cats.syntax.either._
import cats.syntax.eq._
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.i18n.Messages
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.views.LicenceTypeOption

import scala.util.Try

object FormUtils {

  def radioFormFormatter[A : Eq](
    orderedOptions: List[A],
    errorMsgArgs: List[String] = Nil
  ): Formatter[A] =
    new Formatter[A] {
      val optionsZippedWithIndex: List[(A, Int)] = orderedOptions.zipWithIndex

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], A] = {
        lazy val invalidError = FormError(key, "error.invalid", errorMsgArgs)
        data
          .get(key)
          .map(_.trim())
          .filter(_.nonEmpty)
          .fold[Either[Seq[FormError], A]](
            Left(Seq(FormError(key, "error.required", errorMsgArgs)))
          ) { stringValue =>
            Either
              .fromTry(Try(stringValue.toInt))
              .leftMap(_ => Seq(invalidError))
              .flatMap(i =>
                Either
                  .fromOption(
                    optionsZippedWithIndex.find(_._2 === i),
                    Seq(invalidError)
                  )
                  .map(_._1)
              )
          }
      }

      override def unbind(key: String, value: A): Map[String, String] =
        optionsZippedWithIndex
          .find(_._1 === value)
          .fold(Map.empty[String, String]) { case (_, i) =>
            Map(key -> i.toString)
          }
    }

  def licenceTypeFormat(licenceType: LicenceType)(implicit
    messages: Messages
  ): String = {
    val licenceTypeMessage = LicenceTypeOption.licenceTypeOption(licenceType)
    messages(s"licenceType.${licenceTypeMessage.messageKey}")
  }

}
