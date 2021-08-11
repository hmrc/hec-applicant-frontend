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

package uk.gov.hmrc.hecapplicantfrontend.controllers

object DateErrorScenarios {

  final case class DateErrorScenario(
    dayInput: Option[String],
    monthInput: Option[String],
    yearInput: Option[String],
    expectedErrorMessageKey: String
  )

  def dateErrorScenarios(
    dateKey: String
  ): List[DateErrorScenario] =
    List(
      DateErrorScenario(
        None,
        None,
        None,
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        Some(""),
        None,
        None,
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        None,
        Some(""),
        None,
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        None,
        None,
        Some(""),
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        Some(""),
        Some(""),
        None,
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        Some(""),
        None,
        Some(""),
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        None,
        Some(""),
        Some(""),
        s"$dateKey.error.required"
      ),
      DateErrorScenario(
        Some(""),
        Some(""),
        Some(""),
        s"$dateKey.error.required"
      ),
      // single field empty
      DateErrorScenario(
        None,
        Some("12"),
        Some("2020"),
        s"$dateKey.error.dayRequired"
      ),
      DateErrorScenario(
        None,
        Some("100"),
        Some("-1000"),
        s"$dateKey.error.dayRequired"
      ),
      DateErrorScenario(
        Some("1"),
        None,
        Some("2020"),
        s"$dateKey.error.monthRequired"
      ),
      DateErrorScenario(
        Some("-1"),
        None,
        Some("1.2"),
        s"$dateKey.error.monthRequired"
      ),
      DateErrorScenario(
        Some("1"),
        Some("12"),
        None,
        s"$dateKey.error.yearRequired"
      ),
      DateErrorScenario(
        Some("0"),
        Some("-1"),
        None,
        s"$dateKey.error.yearRequired"
      ),
      // two fields mossing
      DateErrorScenario(
        Some("1"),
        None,
        None,
        s"$dateKey.error.monthAndYearRequired"
      ),
      DateErrorScenario(
        Some("0"),
        None,
        None,
        s"$dateKey.error.monthAndYearRequired"
      ),
      DateErrorScenario(
        None,
        Some("12"),
        None,
        s"$dateKey.error.dayAndYearRequired"
      ),
      DateErrorScenario(
        None,
        Some("-1"),
        None,
        s"$dateKey.error.dayAndYearRequired"
      ),
      DateErrorScenario(
        None,
        None,
        Some("2020"),
        s"$dateKey.error.dayAndMonthRequired"
      ),
      DateErrorScenario(
        None,
        None,
        Some("-1"),
        s"$dateKey.error.dayAndMonthRequired"
      ),
      // day invalid and takes precedence over month and year
      DateErrorScenario(
        Some("0"),
        Some("12"),
        Some("2020"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("32"),
        Some("12"),
        Some("2020"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("-1"),
        Some("-1"),
        Some("-2020"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("1.2"),
        Some("3.4"),
        Some("4.5"),
        s"$dateKey.error.invalid"
      ),
      // month invalid and takes precedence over year
      DateErrorScenario(
        Some("1"),
        Some("13"),
        Some("2020"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("1"),
        Some("0"),
        Some("0"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("1"),
        Some("-1"),
        Some("-6"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("1"),
        Some("1.2"),
        Some("3.4"),
        s"$dateKey.error.invalid"
      ),
      // year invalid
      DateErrorScenario(
        Some("1"),
        Some("12"),
        Some("0"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("1"),
        Some("12"),
        Some("-1"),
        s"$dateKey.error.invalid"
      ),
      DateErrorScenario(
        Some("1"),
        Some("12"),
        Some("1.2"),
        s"$dateKey.error.invalid"
      ),
      // date does not exist
      DateErrorScenario(
        Some("31"),
        Some("2"),
        Some("2019"),
        s"$dateKey.error.invalid"
      )
    )

}
