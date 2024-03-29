/*
 * Copyright 2024 HM Revenue & Customs
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
import play.api.libs.json.{JsError, JsString, Json}

class YesNoAnswerSpec extends AnyWordSpec with Matchers {

  "YesNoAnswer" should {

    "write to JSON" when {

      s"the yes/no type is ${YesNoAnswer.Yes}" in {
        Json.toJson[YesNoAnswer](YesNoAnswer.Yes) shouldBe JsString("Yes")
      }

      s"the yes/no type is ${YesNoAnswer.No}" in {
        Json.toJson[YesNoAnswer](YesNoAnswer.No) shouldBe JsString("No")
      }
    }

    "read from JSON" when {

      s"the yes/no type is ${YesNoAnswer.Yes}" in {
        JsString("Yes").as[YesNoAnswer] shouldBe YesNoAnswer.Yes
      }

      s"the yes/no type is ${YesNoAnswer.No}" in {
        JsString("No").as[YesNoAnswer] shouldBe YesNoAnswer.No
      }
    }

    "fail to read from JSON" when {

      val js = JsString("aaaaaaa")

      s"the yes/no answer is not recognised" in {
        js.validate[YesNoAnswer] shouldBe JsError(s"Unknown yes/no answer: ${js.toString()}")

      }
    }
  }

}
