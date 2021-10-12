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

import ai.x.play.json.Jsonx
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.CRN
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}

sealed trait UserAnswersType
object UserAnswersType {
  case object Incomplete extends UserAnswersType
  case object Complete extends UserAnswersType

  import ai.x.play.json.SingletonEncoder.simpleName
  import ai.x.play.json.implicits.formatSingleton

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  implicit val format: Format[UserAnswersType] = Jsonx.formatSealed[UserAnswersType]
}

sealed trait UserAnswers extends Product with Serializable {
  val userAnswersType: UserAnswersType
}

object UserAnswers {

  @Lenses
  final case class IncompleteUserAnswers(
    licenceType: Option[LicenceType],
    licenceTimeTrading: Option[LicenceTimeTrading],
    licenceValidityPeriod: Option[LicenceValidityPeriod],
    taxSituation: Option[TaxSituation],
    saIncomeDeclared: Option[YesNoAnswer],
    entityType: Option[EntityType],
    crn: Option[CRN],
    companyDetailsConfirmed: Option[YesNoAnswer],
    chargeableForCT: Option[YesNoAnswer]
  ) extends UserAnswers {
    val userAnswersType: UserAnswersType = UserAnswersType.Incomplete
  }

  final case class CompleteUserAnswers(
    licenceType: LicenceType,
    licenceTimeTrading: LicenceTimeTrading,
    licenceValidityPeriod: LicenceValidityPeriod,
    taxSituation: Option[TaxSituation],
    saIncomeDeclared: Option[YesNoAnswer],
    entityType: Option[EntityType],
    crn: Option[CRN],
    companyDetailsConfirmed: Option[YesNoAnswer],
    chargeableForCT: Option[YesNoAnswer]
  ) extends UserAnswers {
    val userAnswersType: UserAnswersType = UserAnswersType.Complete
  }

  object IncompleteUserAnswers {

    def fromCompleteAnswers(c: CompleteUserAnswers): IncompleteUserAnswers =
      IncompleteUserAnswers(
        Some(c.licenceType),
        Some(c.licenceTimeTrading),
        Some(c.licenceValidityPeriod),
        c.taxSituation,
        c.saIncomeDeclared,
        c.entityType,
        c.crn,
        c.companyDetailsConfirmed,
        c.chargeableForCT
      )

  }

  implicit class UserAnswersOps(private val u: UserAnswers) extends AnyVal {

    def fold[A](ifIncomplete: IncompleteUserAnswers => A, ifComplete: CompleteUserAnswers => A): A = u match {
      case i: IncompleteUserAnswers => ifIncomplete(i)
      case c: CompleteUserAnswers   => ifComplete(c)
    }

    def unset[A](
      fieldLens: IncompleteUserAnswers.type => Lens[
        IncompleteUserAnswers,
        Option[A]
      ]
    ): IncompleteUserAnswers =
      fieldLens(IncompleteUserAnswers).set(None)(
        fold(identity, IncompleteUserAnswers.fromCompleteAnswers)
      )

  }

  val empty: IncompleteUserAnswers = IncompleteUserAnswers(None, None, None, None, None, None, None, None, None)

  implicit val format: OFormat[UserAnswers] = new OFormat[UserAnswers] {
    override def reads(json: JsValue): JsResult[UserAnswers] =
      (json \ "type")
        .validate[UserAnswersType]
        .flatMap {
          case UserAnswersType.Incomplete => Json.reads[IncompleteUserAnswers].reads(json)
          case UserAnswersType.Complete   => Json.reads[CompleteUserAnswers].reads(json)
        }

    override def writes(o: UserAnswers): JsObject = {
      val json = o match {
        case i: IncompleteUserAnswers => Json.writes[IncompleteUserAnswers].writes(i)
        case c: CompleteUserAnswers   => Json.writes[CompleteUserAnswers].writes(c)
      }
      json ++ Json.obj("type" -> o.userAnswersType)
    }
  }

}
