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

import cats.Eq
import play.api.libs.json.{JsObject, JsResult, JsValue, Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}

import java.time.ZonedDateTime

trait HECSession extends Product with Serializable {
  val entityType: EntityType
  val userAnswers: UserAnswers
  val loginData: LoginData
  val retrievedJourneyData: RetrievedJourneyData
  val completedTaxCheck: Option[HECTaxCheck]
  val taxCheckStartDateTime: Option[ZonedDateTime]
  val unexpiredTaxChecks: List[TaxCheckListItem]

}

object HECSession {

  final case class IndividualHECSession(
    loginData: IndividualLoginData,
    retrievedJourneyData: IndividualRetrievedJourneyData,
    userAnswers: UserAnswers,
    completedTaxCheck: Option[HECTaxCheck],
    taxCheckStartDateTime: Option[ZonedDateTime],
    unexpiredTaxChecks: List[TaxCheckListItem]
  ) extends HECSession {
    override val entityType: EntityType = EntityType.Individual
  }

  object IndividualHECSession {

    def newSession(loginData: IndividualLoginData): IndividualHECSession =
      IndividualHECSession(loginData, IndividualRetrievedJourneyData.empty, UserAnswers.empty, None, None, List.empty)

  }

  final case class CompanyHECSession(
    loginData: CompanyLoginData,
    retrievedJourneyData: CompanyRetrievedJourneyData,
    userAnswers: UserAnswers,
    completedTaxCheck: Option[HECTaxCheck],
    taxCheckStartDateTime: Option[ZonedDateTime],
    unexpiredTaxChecks: List[TaxCheckListItem]
  ) extends HECSession {
    override val entityType: EntityType = EntityType.Company
  }

  object CompanyHECSession {

    def newSession(loginData: CompanyLoginData): CompanyHECSession =
      CompanyHECSession(loginData, CompanyRetrievedJourneyData.empty, UserAnswers.empty, None, None, List.empty)

  }

  def newSession(loginData: LoginData): HECSession = loginData match {
    case i: IndividualLoginData => IndividualHECSession.newSession(i)
    case c: CompanyLoginData    => CompanyHECSession.newSession(c)
  }

  implicit class HECSessionOps(private val s: HECSession) extends AnyVal {

    def fold[A](ifIndividual: IndividualHECSession => A, ifCompany: CompanyHECSession => A): A = s match {
      case i: IndividualHECSession => ifIndividual(i)
      case c: CompanyHECSession    => ifCompany(c)
    }

    def mapAsIndividual[A](f: IndividualHECSession => A): A =
      s.fold(
        f,
        _ => sys.error("Expected individual session data but got company session data")
      )

    def mapAsCompany[A](f: CompanyHECSession => A): A =
      s.fold(
        _ => sys.error("Expected company session data but got individual session data"),
        f
      )

  }

  implicit val eq: Eq[HECSession] = Eq.fromUniversalEquals

  implicit val format: OFormat[HECSession] = new OFormat[HECSession] {
    override def reads(json: JsValue): JsResult[HECSession] =
      (json \ "type")
        .validate[EntityType]
        .flatMap {
          case EntityType.Individual => Json.reads[IndividualHECSession].reads(json)
          case EntityType.Company    => Json.reads[CompanyHECSession].reads(json)
        }

    override def writes(o: HECSession): JsObject = {
      val json = o match {
        case i: IndividualHECSession => Json.writes[IndividualHECSession].writes(i)
        case c: CompanyHECSession    => Json.writes[CompanyHECSession].writes(c)
      }
      json ++ Json.obj("type" -> o.entityType)
    }
  }

}
