/*
 * Copyright 2023 HM Revenue & Customs
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
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

class UserAnswersSpec extends AnyWordSpec with Matchers {

  "IndividualUserAnswers" must {

    "have an empty val" in {
      IndividualUserAnswers.empty shouldBe Fixtures.incompleteIndividualUserAnswers()
    }

    "have a fold method" which {

      "works with incomplete answers" in {
        val incompleteAnswers = IndividualUserAnswers.empty
        incompleteAnswers.fold(
          _ shouldBe incompleteAnswers,
          _ => fail()
        )
      }

      "works with complete answers" in {
        val completeAnswers = Fixtures.completeIndividualUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.TwoToFourYears,
          LicenceValidityPeriod.UpToFiveYears,
          TaxSituation.PAYE,
          Some(YesNoAnswer.Yes),
          Some(EntityType.Individual)
        )
        completeAnswers.fold(
          _ => fail(),
          _ shouldBe completeAnswers
        )
      }

    }

    "have a method which converts complete answers to incomplete" in {
      val completeAnswers = Fixtures.completeIndividualUserAnswers(
        LicenceType.DriverOfTaxisAndPrivateHires,
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToTwoYears,
        TaxSituation.PAYE,
        Some(YesNoAnswer.Yes),
        Some(EntityType.Individual)
      )
      IncompleteIndividualUserAnswers.fromCompleteAnswers(completeAnswers) shouldBe Fixtures
        .incompleteIndividualUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.TwoToFourYears),
          Some(LicenceValidityPeriod.UpToTwoYears),
          Some(TaxSituation.PAYE),
          Some(YesNoAnswer.Yes),
          Some(EntityType.Individual)
        )

    }

    "have an unset method" which {

      val incompleteAnswers =
        Fixtures.incompleteIndividualUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.ZeroToTwoYears),
          Some(LicenceValidityPeriod.UpToThreeYears),
          Some(TaxSituation.PAYE),
          Some(YesNoAnswer.Yes),
          Some(EntityType.Company)
        )

      val completeAnswers =
        Fixtures.completeIndividualUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          TaxSituation.PAYE,
          Some(YesNoAnswer.Yes),
          Some(EntityType.Company)
        )

      "unsets the licence type field" in {
        incompleteAnswers.unset(_.licenceTypeLens) shouldBe incompleteAnswers.copy(licenceType = None)
        completeAnswers.unset(_.licenceTypeLens)   shouldBe incompleteAnswers.copy(licenceType = None)
      }

      "unsets the licence time trading field" in {
        incompleteAnswers.unset(_.licenceTimeTradingLens) shouldBe incompleteAnswers.copy(licenceTimeTrading = None)
        completeAnswers.unset(_.licenceTimeTradingLens)   shouldBe incompleteAnswers.copy(licenceTimeTrading = None)
      }

      "unsets the licence validity period field" in {
        incompleteAnswers.unset(_.licenceValidityPeriodLens) shouldBe incompleteAnswers.copy(licenceValidityPeriod =
          None
        )
        completeAnswers.unset(_.licenceValidityPeriodLens)   shouldBe incompleteAnswers.copy(licenceValidityPeriod = None)
      }

      "unsets the entity type field" in {
        incompleteAnswers.unset(_.entityTypeLens) shouldBe incompleteAnswers.copy(entityType = None)
        completeAnswers.unset(_.entityTypeLens)   shouldBe incompleteAnswers.copy(entityType = None)
      }

    }

    "perform JSON de/serialisation correctly" must {
      val incompleteAnswers: IndividualUserAnswers =
        Fixtures.incompleteIndividualUserAnswers(
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

      val completeAnswers: IndividualUserAnswers =
        Fixtures.completeIndividualUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          TaxSituation.PAYE,
          Some(YesNoAnswer.Yes),
          Some(EntityType.Company)
        )

      val completeJson = Json.parse("""{
                                      |"licenceType":"DriverOfTaxisAndPrivateHires",
                                      |"licenceTimeTrading":"ZeroToTwoYears",
                                      |"licenceValidityPeriod":"UpToThreeYears",
                                      |"taxSituation":"PAYE",
                                      |"saIncomeDeclared": "Yes",
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
        Json.fromJson[IndividualUserAnswers](incompleteJson).get shouldBe incompleteAnswers
      }

      "deserialize Complete answers" in {
        Json.fromJson[IndividualUserAnswers](completeJson).get shouldBe completeAnswers
      }
    }
  }

  "CompanyUserAnswers" must {

    "have an empty val" in {
      CompanyUserAnswers.empty shouldBe Fixtures.incompleteCompanyUserAnswers()
    }

    "have a fold method" which {

      "works with incomplete answers" in {
        val incompleteAnswers = CompanyUserAnswers.empty
        incompleteAnswers.fold(
          _ shouldBe incompleteAnswers,
          _ => fail()
        )
      }

      "works with complete answers" in {
        val completeAnswers = Fixtures.completeCompanyUserAnswers()
        completeAnswers.fold(
          _ => fail(),
          _ shouldBe completeAnswers
        )
      }

    }

    "have a method which converts complete answers to incomplete" in {
      val completeAnswers = Fixtures.completeCompanyUserAnswers(
        LicenceType.DriverOfTaxisAndPrivateHires,
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToTwoYears,
        Some(EntityType.Company),
        CRN("crn"),
        YesNoAnswer.Yes
      )
      IncompleteCompanyUserAnswers.fromCompleteAnswers(completeAnswers) shouldBe Fixtures
        .incompleteCompanyUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.TwoToFourYears),
          Some(LicenceValidityPeriod.UpToTwoYears),
          Some(EntityType.Company),
          Some(CRN("crn")),
          Some(YesNoAnswer.Yes)
        )

    }

    "have an unset method" which {

      val incompleteAnswers =
        Fixtures.incompleteCompanyUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.ZeroToTwoYears),
          Some(LicenceValidityPeriod.UpToThreeYears),
          Some(EntityType.Company),
          Some(CRN("crn")),
          Some(YesNoAnswer.Yes)
        )

      val completeAnswers =
        Fixtures.completeCompanyUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          Some(EntityType.Company),
          CRN("crn"),
          YesNoAnswer.Yes
        )

      "unsets the licence type field" in {
        incompleteAnswers.unset(_.licenceTypeLens) shouldBe incompleteAnswers.copy(licenceType = None)
        completeAnswers.unset(_.licenceTypeLens)   shouldBe incompleteAnswers.copy(licenceType = None)
      }

      "unsets the licence time trading field" in {
        incompleteAnswers.unset(_.licenceTimeTradingLens) shouldBe incompleteAnswers.copy(licenceTimeTrading = None)
        completeAnswers.unset(_.licenceTimeTradingLens)   shouldBe incompleteAnswers.copy(licenceTimeTrading = None)
      }

      "unsets the licence validity period field" in {
        incompleteAnswers.unset(_.licenceValidityPeriodLens) shouldBe incompleteAnswers.copy(licenceValidityPeriod =
          None
        )
        completeAnswers.unset(_.licenceValidityPeriodLens)   shouldBe incompleteAnswers.copy(licenceValidityPeriod = None)
      }

      "unsets the entity type field" in {
        incompleteAnswers.unset(_.entityTypeLens) shouldBe incompleteAnswers.copy(entityType = None)
        completeAnswers.unset(_.entityTypeLens)   shouldBe incompleteAnswers.copy(entityType = None)
      }

    }

    "perform JSON de/serialisation correctly" must {
      val incompleteAnswers: CompanyUserAnswers =
        Fixtures.incompleteCompanyUserAnswers(
          Some(LicenceType.DriverOfTaxisAndPrivateHires),
          Some(LicenceTimeTrading.ZeroToTwoYears),
          Some(LicenceValidityPeriod.UpToThreeYears),
          Some(EntityType.Company),
          Some(CRN("crn")),
          Some(YesNoAnswer.Yes)
        )

      val incompleteJson = Json.parse("""{
                                        |"licenceType":"DriverOfTaxisAndPrivateHires",
                                        |"licenceTimeTrading":"ZeroToTwoYears",
                                        |"licenceValidityPeriod":"UpToThreeYears",
                                        |"entityType":"Company",
                                        |"crn": "crn",
                                        |"companyDetailsConfirmed": "Yes",
                                        |"type":"Incomplete"
                                        |}""".stripMargin)

      val completeAnswers: CompanyUserAnswers =
        Fixtures.completeCompanyUserAnswers(
          LicenceType.DriverOfTaxisAndPrivateHires,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToThreeYears,
          Some(EntityType.Company),
          CRN("crn"),
          YesNoAnswer.Yes
        )

      val completeJson = Json.parse("""{
                                      |"licenceType":"DriverOfTaxisAndPrivateHires",
                                      |"licenceTimeTrading":"ZeroToTwoYears",
                                      |"licenceValidityPeriod":"UpToThreeYears",
                                      |"entityType":"Company",
                                      |"crn": "crn",
                                      |"companyDetailsConfirmed": "Yes",
                                      |"type":"Complete"
                                      |}""".stripMargin)

      "serialize Incomplete answers" in {
        Json.toJson(incompleteAnswers) shouldBe incompleteJson
      }

      "serialize Complete answers" in {
        Json.toJson(completeAnswers) shouldBe completeJson
      }

      "deserialize Incomplete answers" in {
        Json.fromJson[CompanyUserAnswers](incompleteJson).get shouldBe incompleteAnswers
      }

      "deserialize Complete answers" in {
        Json.fromJson[CompanyUserAnswers](completeJson).get shouldBe completeAnswers
      }
    }
  }
}
