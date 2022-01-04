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

package uk.gov.hmrc.hecapplicantfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CTUTR

class CTUTRSpec extends AnyWordSpec with Matchers {

  "CTUTR" must {

    def ctutrWithDigits(length: Int) = "1" * length

    val ctutr10Digits = ctutrWithDigits(10)
    val ctutr13Digits = ctutrWithDigits(13)

    "return as is for 10 digit string" in {
      CTUTR.strip(ctutr10Digits) shouldBe ctutr10Digits
    }

    "strip the first char if starts with a k/K followed by a 10 digit string" in {
      CTUTR.strip(s"k$ctutr10Digits") shouldBe ctutr10Digits
      CTUTR.strip(s"K$ctutr10Digits") shouldBe ctutr10Digits
    }

    "strip the last char if ends with a k/K preceeded by a 10 digit string" in {
      CTUTR.strip(s"${ctutr10Digits}k") shouldBe ctutr10Digits
      CTUTR.strip(s"${ctutr10Digits}K") shouldBe ctutr10Digits
    }

    "strip the first 3 chars for a 13 digit string" in {
      CTUTR.strip(ctutr13Digits) shouldBe ctutr10Digits
    }

    "strip the first char if starts with a K followed by a 13 digit string" in {
      CTUTR.strip(s"k$ctutr13Digits") shouldBe ctutr10Digits
      CTUTR.strip(s"K$ctutr13Digits") shouldBe ctutr10Digits
    }

    "strip the last char if ends with a k/K preceeded by a 13 digit string" in {
      CTUTR.strip(s"${ctutr13Digits}k") shouldBe ctutr10Digits
      CTUTR.strip(s"${ctutr13Digits}K") shouldBe ctutr10Digits
    }

    "return the string as is otherwise" in {
      CTUTR.strip(ctutrWithDigits(5))  shouldBe ctutrWithDigits(5)
      CTUTR.strip(ctutrWithDigits(11)) shouldBe ctutrWithDigits(11)
      CTUTR.strip(ctutrWithDigits(14)) shouldBe ctutrWithDigits(14)
    }
  }

}
