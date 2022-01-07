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
import play.api.libs.json.{JsString, JsSuccess, Json}

import java.time.LocalDate

class DateOfBirthSpec extends AnyWordSpec with Matchers {

  "DateOfBirth" must {

    "have a format instance" in {
      val dob = DateOfBirth(LocalDate.of(2012, 5, 31))

      val json = Json.toJson(dob)
      json                             shouldBe JsString("2012-05-31")
      Json.fromJson[DateOfBirth](json) shouldBe JsSuccess(dob)
    }

  }

}
