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
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

class UserAnswersSpec extends AnyWordSpec with Matchers {

  "UserAnswers" must {

    "have an empty val" in {
      UserAnswers.empty shouldBe IncompleteUserAnswers(None)
    }

    "have a fold method" which {

      "works with incomplete answers" in {
        val incompleteAnswers = IncompleteUserAnswers(Some(LicenceType.ScrapMetalDealerSite))
        incompleteAnswers.fold(
          _ shouldBe incompleteAnswers,
          _ => fail()
        )
      }

      "works with complete answers" in {
        val completeAnswers = CompleteUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceExpiryDate(TimeUtils.today().minusDays(10L))
        )
        completeAnswers.fold(
          _ => fail(),
          _ shouldBe completeAnswers
        )
      }

    }

    "have a method which converts complete answers to incomplete" in {
      val completeAnswers       = CompleteUserAnswers(
        LicenceType.DriverOfTaxisAndPrivateHires,
        LicenceExpiryDate(TimeUtils.today().minusDays(10L))
      )
      val incompleteUserAnswers = IncompleteUserAnswers.fromCompleteAnswers(completeAnswers)

      incompleteUserAnswers.licenceType shouldBe Some(LicenceType.DriverOfTaxisAndPrivateHires)
    }

    "have an unset method" which {

      "unsets the licence type field" in {
        IncompleteUserAnswers(Some(LicenceType.DriverOfTaxisAndPrivateHires))
          .unset(_.licenceType)                           shouldBe UserAnswers.empty
        CompleteUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceExpiryDate(TimeUtils.today().minusDays(10L))
        ).unset(_.licenceType).unset(_.licenceExpiryDate) shouldBe UserAnswers.empty
      }

    }

  }

}
