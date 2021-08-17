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

package uk.gov.hmrc.hecapplicantfrontend.utils

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.hecapplicantfrontend.models.TaxDisplayYear
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

import java.time.LocalDate

class TimeUtilsSpec extends AnyWordSpec with Matchers {

  "tax display year" must {

    "be 2019 " when {

      "current year is 2021 and six months before current date is before 6 april 2021" in {
        val result   = TimeUtils.getTaxYearDisplayDate(LocalDate.of(2021, 8, 17))
        val expected = TaxDisplayYear(2019)
        result shouldBe expected
      }


      "current year is 2020 and six months before current date is after 6 april 2020" in {
        val result   = TimeUtils.getTaxYearDisplayDate(LocalDate.of(2020, 12, 17))
        val expected = TaxDisplayYear(2019)
        result shouldBe expected
      }

      "current year is 2020 and six months before current date is exactly at 6 april 2020" in {
        val result   = TimeUtils.getTaxYearDisplayDate(LocalDate.of(2020, 10, 6))
        val expected = TaxDisplayYear(2019)
        result shouldBe expected
      }
    }

    "be 2020 " when {

      "current year is 2021 and six months before current date is after 6 april 2021" in {
        val result   = TimeUtils.getTaxYearDisplayDate(LocalDate.of(2021, 12, 17))
        val expected = TaxDisplayYear(2020)
        result shouldBe expected
      }

      "current year is 2021 and six months before current date is exactly 6 april 2020" in {
        val result   = TimeUtils.getTaxYearDisplayDate(LocalDate.of(2021, 10, 6))
        val expected = TaxDisplayYear(2020)
        result shouldBe expected
      }

      "current year is 2022 and six months before current date is before 6 april 2022" in {
        val result   = TimeUtils.getTaxYearDisplayDate(LocalDate.of(2022, 10, 5))
        val expected = TaxDisplayYear(2020)
        result shouldBe expected
      }
    }

  }
}
