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
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType.Individual
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.{LocalDate, ZonedDateTime}

class HECSessionSpec extends AnyWordSpec with Matchers {

  "HECSession" when {

    "newSession is called" should {

      "create an IndividualHECSession if given Individual login data" in {

        HECSession.newSession(Foo.loginData) shouldBe an[IndividualHECSession]
      }

      "create an IndividualHECSession if given Company login data" in {

        val loginData = CompanyLoginData(GGCredId(""), None, None, None)

        HECSession.newSession(loginData) shouldBe an[CompanyHECSession]
      }
    }
  }

  "HECSessionOps" should {

    "perform fold operation successfully" when {

      "session type is Individual" in {
        Fixtures.individualHECSession().fold(_ => true, _ => false) shouldBe true
      }

      "session type is Company" in {
        Fixtures.companyHECSession().fold(_ => false, _ => true) shouldBe true
      }
    }

    "throw InconsistentSessionState" when {

      "session type is invalid on .fold" in {

        an[InconsistentSessionState] shouldBe thrownBy {
          Foo.fold(_ => 1, _ => 2)
        }
      }

      "session type is invalid on .ensureLicenceTypePresent" in {

        an[InconsistentSessionState] shouldBe thrownBy {
          Foo.ensureLicenceTypePresent(_ => true)
        }
      }

      "session type is invalid on .writes" in {

        an[InconsistentSessionState] shouldBe thrownBy {
          Json.toJson[HECSession](Foo)
        }
      }
    }

    "ensure that the license type is present" when {

      "session type is individual" in {

        Fixtures.individualHECSession()
          .copy(userAnswers = Fixtures.completeIndividualUserAnswers())
          .ensureLicenceTypePresent(_ => true) shouldBe true
      }

      "session type is company" in {

        Fixtures.companyHECSession()
          .copy(userAnswers = Fixtures.completeCompanyUserAnswers())
          .ensureLicenceTypePresent(_ => true) shouldBe true
      }
    }
  }
}

object Foo extends HECSession {
  override val entityType: EntityType = Individual
  override val userAnswers: UserAnswers = IncompleteIndividualUserAnswers(None, None, None, None, None, None)
  override val loginData: LoginData = IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)
  override val retrievedJourneyData: RetrievedJourneyData = IndividualRetrievedJourneyData(None)
  override val completedTaxCheck: Option[HECTaxCheck] = None
  override val taxCheckStartDateTime: Option[ZonedDateTime] = None
  override val unexpiredTaxChecks: List[TaxCheckListItem] = Nil
  override val emailRequestedForTaxCheck: Option[EmailRequestedForTaxCheck] = None
  override val hasResentEmailConfirmation: Boolean = false
  override val userEmailAnswers: Option[UserEmailAnswers] = None
  override val showUserResearchBanner: Option[Boolean] = None

  override def productArity: Int = 0

  override def productElement(n: Int): Any = ()

  override def canEqual(that: Any): Boolean = false
}
