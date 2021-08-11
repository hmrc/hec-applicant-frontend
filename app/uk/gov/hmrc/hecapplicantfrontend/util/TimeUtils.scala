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

package uk.gov.hmrc.hecapplicantfrontend.util

import cats.syntax.either._
import play.api.data.FormError
import play.api.data.format.Formatter
import play.api.i18n.Messages

import java.time.{Clock, LocalDate}
import scala.util.Try

object TimeUtils {

  val clock: Clock = Clock.systemUTC()

  def today(): LocalDate = LocalDate.now(clock)

  def dateFormatter(
    maximumDateInclusive: Option[LocalDate],
    minimumDateInclusive: Option[LocalDate],
    dayKey: String,
    monthKey: String,
    yearKey: String,
    dateKey: String,
    extraValidation: List[LocalDate => Either[FormError, Unit]] = List.empty,
    tooFarInFutureArgs: Seq[String] = Seq.empty,
    tooFarInPastArgs: Seq[String] = Seq.empty
  ): Formatter[LocalDate] =
    new Formatter[LocalDate] {
      def dateFieldStringValues(
        data: Map[String, String]
      ): Either[FormError, (String, String, String)] =
        List(dayKey, monthKey, yearKey)
          .map(data.get(_).map(_.trim).filter(_.nonEmpty)) match {
          case Some(dayString) :: Some(monthString) :: Some(
                yearString
              ) :: Nil =>
            Right((dayString, monthString, yearString))
          case None :: Some(_) :: Some(_) :: Nil =>
            Left(FormError(dateKey, "error.dayRequired"))
          case Some(_) :: None :: Some(_) :: Nil =>
            Left(FormError(dateKey, "error.monthRequired"))
          case Some(_) :: Some(_) :: None :: Nil =>
            Left(FormError(dateKey, "error.yearRequired"))
          case Some(_) :: None :: None :: Nil    =>
            Left(FormError(dateKey, "error.monthAndYearRequired"))
          case None :: Some(_) :: None :: Nil    =>
            Left(FormError(dateKey, "error.dayAndYearRequired"))
          case None :: None :: Some(_) :: Nil    =>
            Left(FormError(dateKey, "error.dayAndMonthRequired"))
          case _                                 => Left(FormError(dateKey, "error.required"))
        }

      def toValidInt(
        stringValue: String,
        maxValue: Option[Int]
      ): Either[FormError, Int] =
        Either.fromOption(
          Try(BigDecimal(stringValue).toIntExact).toOption.filter(i => i > 0 && maxValue.forall(i <= _)),
          FormError(dateKey, "error.invalid")
        )

      override def bind(
        key: String,
        data: Map[String, String]
      ): Either[Seq[FormError], LocalDate] = {
        val result = for {
          dateFieldStrings <- dateFieldStringValues(data)
          day ← toValidInt(dateFieldStrings._1, Some(31))
          month ← toValidInt(dateFieldStrings._2, Some(12))
          year ← toValidInt(dateFieldStrings._3, None)
          date ← Either
                   .fromTry(Try(LocalDate.of(year, month, day)))
                   .leftMap(_ => FormError(dateKey, "error.invalid"))
                   .flatMap(date =>
                     if (maximumDateInclusive.exists(_.isBefore(date)))
                       Left(FormError(dateKey, "error.tooFarInFuture", tooFarInFutureArgs))
                     else if (minimumDateInclusive.exists(_.isAfter(date)))
                       Left(FormError(dateKey, "error.tooFarInPast", tooFarInPastArgs))
                     else
                       extraValidation
                         .map(_(date))
                         .find(_.isLeft)
                         .getOrElse(Right(()))
                         .map(_ => date)
                   )
        } yield date

        result.leftMap(Seq(_))
      }

      override def unbind(key: String, value: LocalDate): Map[String, String] =
        Map(
          dayKey   -> value.getDayOfMonth.toString,
          monthKey -> value.getMonthValue.toString,
          yearKey  -> value.getYear.toString
        )

    }

  def govDisplayFormat(date: LocalDate)(implicit messages: Messages): String =
    s"""${date.getDayOfMonth()} ${messages(
      s"date.${date.getMonthValue()}"
    )} ${date.getYear()}"""

  implicit class LocalDateOps(private val d: LocalDate) extends AnyVal {

    def isAfterOrOn(other: LocalDate): Boolean =
      d.isEqual(other) || d.isAfter(other)

  }

}
