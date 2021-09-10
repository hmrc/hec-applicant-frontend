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
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceExpiryDate, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

class UserAnswersSpec extends AnyWordSpec with Matchers {

  "UserAnswers" must {

    "have an empty val" in {
      UserAnswers.empty shouldBe IncompleteUserAnswers(None, None, None, None, None, None, None)
    }

    "have a fold method" which {

      "works with incomplete answers" in {
        val incompleteAnswers = UserAnswers.empty
        incompleteAnswers.fold(
          _ shouldBe incompleteAnswers,
          _ => fail()
        )
      }

      "works with complete answers" in {
        val completeAnswers = CompleteUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
          LicenceTimeTrading.TwoToFourYears,
          LicenceValidityPeriod.UpToFiveYears,
          TaxSituation.PAYE,
          IncomeConfirmation.Yes,
          Some(EntityType.Individual)
        )
        completeAnswers.fold(
          _ => fail(),
          _ shouldBe completeAnswers
        )
      }

    }

    "have a method which converts complete answers to incomplete" in {
      val completeAnswers = CompleteUserAnswers(
        LicenceType.DriverOfTaxisAndPrivateHires,
        LicenceExpiryDate(TimeUtils.today()),
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToTwoYears,
        TaxSituation.PAYE,
        IncomeConfirmation.Yes,
        Some(EntityType.Individual)
      )
      IncompleteUserAnswers.fromCompleteAnswers(completeAnswers) shouldBe IncompleteUserAnswers(
        Some(LicenceType.DriverOfTaxisAndPrivateHires),
        Some(LicenceExpiryDate(TimeUtils.today())),
        Some(LicenceTimeTrading.TwoToFourYears),
        Some(LicenceValidityPeriod.UpToTwoYears),
        Some(TaxSituation.PAYE),
        Some(IncomeConfirmation.Yes),
        Some(EntityType.Individual)
      )

    }

    "have an unset method" which {

      val incompleteAnswers =
        IncompleteUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceExpiryDate(TimeUtils.today())),
          Some(LicenceTimeTrading.ZeroToTwoYears),
          Some(LicenceValidityPeriod.UpToThreeYears),
          Some(TaxSituation.PAYE),
          Some(IncomeConfirmation.Yes),
          Some(EntityType.Company)
        )

      val completeAnswers =
        CompleteUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceExpiryDate(TimeUtils.today()),
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          TaxSituation.PAYE,
          IncomeConfirmation.Yes,
          Some(EntityType.Company)
        )

      "unsets the licence type field" in {
        incompleteAnswers.unset(_.licenceType) shouldBe incompleteAnswers.copy(licenceType = None)
        completeAnswers.unset(_.licenceType)   shouldBe incompleteAnswers.copy(licenceType = None)
      }

      "unsets the licence Expiry Date field" in {
        incompleteAnswers.unset(_.licenceExpiryDate) shouldBe incompleteAnswers.copy(licenceExpiryDate = None)
        completeAnswers.unset(_.licenceExpiryDate)   shouldBe incompleteAnswers.copy(licenceExpiryDate = None)
      }

      "unsets the licence time trading field" in {
        incompleteAnswers.unset(_.licenceTimeTrading) shouldBe incompleteAnswers.copy(licenceTimeTrading = None)
        completeAnswers.unset(_.licenceTimeTrading)   shouldBe incompleteAnswers.copy(licenceTimeTrading = None)
      }

      "unsets the licence validity period field" in {
        incompleteAnswers.unset(_.licenceValidityPeriod) shouldBe incompleteAnswers.copy(licenceValidityPeriod = None)
        completeAnswers.unset(_.licenceValidityPeriod)   shouldBe incompleteAnswers.copy(licenceValidityPeriod = None)
      }

      "unsets the entity type field" in {
        incompleteAnswers.unset(_.entityType) shouldBe incompleteAnswers.copy(entityType = None)
        completeAnswers.unset(_.entityType)   shouldBe incompleteAnswers.copy(entityType = None)
      }

    }

  }

}
