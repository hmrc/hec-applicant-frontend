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

package uk.gov.hmrc.hecapplicantfrontend.models.emailVerification

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.{EmailAddress, Language}

class PasscodeRequestSpec extends AnyWordSpec with Matchers {

  "PasscodeRequest" must {

    "have a write instance" in {
      val passcodeRequest = PasscodeRequest(EmailAddress("email@test.com"), "service", Language.English)
      Json.toJson(passcodeRequest) shouldBe Json.parse(
        """
          |{
          |  "email": "email@test.com",
          |  "serviceName": "service",
          |  "lang": "en"
          |}
          |""".stripMargin
      )

    }

  }

}
