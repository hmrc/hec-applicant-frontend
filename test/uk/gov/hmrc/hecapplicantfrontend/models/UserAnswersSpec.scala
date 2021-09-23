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
import play.api.libs.json.Json
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}

class UserAnswersSpec extends AnyWordSpec with Matchers {

  "UserAnswers" must {

    "have an empty val" in {
      UserAnswers.empty shouldBe IncompleteUserAnswers(None, None, None, None, None, None, None, None)
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
          LicenceTimeTrading.TwoToFourYears,
          LicenceValidityPeriod.UpToFiveYears,
          TaxSituation.PAYE,
          None,
          Some(EntityType.Individual),
          None,
          None
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
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToTwoYears,
        TaxSituation.PAYE,
        None,
        Some(EntityType.Individual),
        None,
        None
      )
      IncompleteUserAnswers.fromCompleteAnswers(completeAnswers) shouldBe IncompleteUserAnswers(
        Some(LicenceType.DriverOfTaxisAndPrivateHires),
        Some(LicenceTimeTrading.TwoToFourYears),
        Some(LicenceValidityPeriod.UpToTwoYears),
        Some(TaxSituation.PAYE),
        None,
        Some(EntityType.Individual),
        None,
        None
      )

    }

    "have an unset method" which {

      val incompleteAnswers =
        IncompleteUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.ZeroToTwoYears),
          Some(LicenceValidityPeriod.UpToThreeYears),
          Some(TaxSituation.PAYE),
          None,
          Some(EntityType.Company),
          None,
          None
        )

      val completeAnswers =
        CompleteUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          TaxSituation.PAYE,
          None,
          Some(EntityType.Company),
          None,
          None
        )

      "unsets the licence type field" in {
        incompleteAnswers.unset(_.licenceType) shouldBe incompleteAnswers.copy(licenceType = None)
        completeAnswers.unset(_.licenceType)   shouldBe incompleteAnswers.copy(licenceType = None)
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

    "perform JSON de/serialisation correctly" must {
      val incompleteAnswers: UserAnswers =
        IncompleteUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.ZeroToTwoYears),
          Some(LicenceValidityPeriod.UpToThreeYears),
          Some(TaxSituation.PAYE),
          None,
          Some(EntityType.Company)
        )

      val incompleteJson = Json.parse("""{
                                        |"licenceType":"DriverOfTaxisAndPrivateHires",
                                        |"licenceTimeTrading":"ZeroToTwoYears",
                                        |"licenceValidityPeriod":"UpToThreeYears",
                                        |"taxSituation":"PAYE",
                                        |"entityType":"Company",
                                        |"type":"Incomplete"
                                        |}""".stripMargin)

      val completeAnswers: UserAnswers =
        CompleteUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          TaxSituation.PAYE,
          None,
          Some(EntityType.Company)
        )

      val completeJson = Json.parse("""{
                                      |"licenceType":"DriverOfTaxisAndPrivateHires",
                                      |"licenceTimeTrading":"ZeroToTwoYears",
                                      |"licenceValidityPeriod":"UpToThreeYears",
                                      |"taxSituation":"PAYE",
                                      |"entityType":"Company",
                                      |"type":"Complete"
                                      |}""".stripMargin)

      "serialize Incomplete answers" in {
        Json.toJson(incompleteAnswers) shouldBe incompleteJson
      }

      "serialize Complete answers" in {
        Json.toJson(completeAnswers) shouldBe completeJson
      }

      "deserialize Incomplete answers" in {
        Json.fromJson[UserAnswers](incompleteJson).get shouldBe incompleteAnswers
      }

      "deserialize Complete answers" in {
        Json.fromJson[UserAnswers](completeJson).get shouldBe completeAnswers
      }
    }
  }

}
