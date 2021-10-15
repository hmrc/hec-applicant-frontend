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

package uk.gov.hmrc.hecapplicantfrontend.models

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CTUTR

class CTUTRSpec extends AnyWordSpec with Matchers {

  "CTUTR" must {

    "return as is for 10 digit string" in {
      CTUTR.strip("1111111111") shouldBe "1111111111"
    }

    "strip the first char if starts with a k followed by a 10 digit string" in {
      CTUTR.strip("k1111111111") shouldBe "1111111111"
    }

    "strip the last char if ends with a k preceeded by a 10 digit string" in {
      CTUTR.strip("1111111111k") shouldBe "1111111111"
    }

    "strip the first 3 chars for a 13 digit string" in {
      CTUTR.strip("1111111111k") shouldBe "1111111111"
    }

    "strip the first char if starts with a k followed by a 13 digit string" in {
      CTUTR.strip("k1111111111111") shouldBe "1111111111"
    }

    "strip the last char if ends with a k preceeded by a 13 digit string" in {
      CTUTR.strip("1111111111111k") shouldBe "1111111111"
    }

    "throw for everything else" in {
      assertThrows[RuntimeException](CTUTR.strip("1".repeat(5)))
      assertThrows[RuntimeException](CTUTR.strip("1".repeat(11)))
      assertThrows[RuntimeException](CTUTR.strip("1".repeat(14)))
//      assertThrows[RuntimeException](CTUTR.strip("1".repeat(20)))
    }
  }

}
