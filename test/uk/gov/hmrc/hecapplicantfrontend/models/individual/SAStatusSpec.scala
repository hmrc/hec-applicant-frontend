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

package uk.gov.hmrc.hecapplicantfrontend.models.individual

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.{JsString, Json}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.SAStatus

class SAStatusSpec extends AnyWordSpec with Matchers {

  "SAStatus" should {

    "write to JSON" when {

      s"the SA status is ${SAStatus.ReturnFound}" in {
        Json.toJson[SAStatus](SAStatus.ReturnFound) shouldBe JsString("ReturnFound")
      }

      s"the SA status is ${SAStatus.NoticeToFileIssued}" in {
        Json.toJson[SAStatus](SAStatus.NoticeToFileIssued) shouldBe JsString("NoticeToFileIssued")
      }

      s"the SA status is ${SAStatus.NoReturnFound}" in {
        Json.toJson[SAStatus](SAStatus.NoReturnFound) shouldBe JsString("NoReturnFound")
      }
    }

    "read from JSON" when {

      s"the SA status is ${SAStatus.ReturnFound}" in {
        JsString("ReturnFound").as[SAStatus] shouldBe SAStatus.ReturnFound
      }

      s"the SA status is ${SAStatus.NoticeToFileIssued}" in {
        JsString("NoticeToFileIssued").as[SAStatus] shouldBe SAStatus.NoticeToFileIssued
      }

      s"the SA status is ${SAStatus.NoReturnFound}" in {
        JsString("NoReturnFound").as[SAStatus] shouldBe SAStatus.NoReturnFound
      }
    }
  }

}
