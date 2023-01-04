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

import play.api.i18n.Messages

import java.time.format.DateTimeFormatter
import java.time.{Clock, LocalDate, ZoneId, ZonedDateTime}

object TimeUtils {

  val clock: Clock = Clock.systemUTC()

  def today(): LocalDate = LocalDate.now(clock)

  def now(): ZonedDateTime = ZonedDateTime.now(ZoneId.of("Europe/London"))

  def govDisplayFormat(date: LocalDate)(implicit messages: Messages): String = {
    val dayOfMonth = date.getDayOfMonth
    val month      = messages(s"date.${date.getMonthValue}")
    val year       = date.getYear

    s"$dayOfMonth $month $year"
  }

  private def getAmPm(date: ZonedDateTime) = if (date.getHour >= 12) "afterNoon" else "beforeNoon"

  private def formatHourMinutes(time: ZonedDateTime): String =
    time.format(DateTimeFormatter.ofPattern("h:mm"))

  def govDateTimeDisplayFormat(date: ZonedDateTime)(implicit messages: Messages): String = {
    val formattedDate = govDisplayFormat(date.toLocalDate)
    val hourMin       = formatHourMinutes(date)
    val amOrPm        = messages(s"date.${getAmPm(date)}")
    s"""$formattedDate $hourMin$amOrPm"""
  }

}
