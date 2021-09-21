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
import ai.x.play.json.SingletonEncoder.simpleName
import ai.x.play.json.implicits.formatSingleton
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.{Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}

sealed trait UserAnswers extends Product with Serializable

object UserAnswers {

  @Lenses
  final case class IncompleteUserAnswers(
    licenceType: Option[LicenceType],
    licenceTimeTrading: Option[LicenceTimeTrading],
    licenceValidityPeriod: Option[LicenceValidityPeriod],
    taxSituation: Option[TaxSituation],
    saIncomeDeclared: Option[IncomeDeclared],
    entityType: Option[EntityType]
  ) extends UserAnswers

  final case class CompleteUserAnswers(
    licenceType: LicenceType,
    licenceTimeTrading: LicenceTimeTrading,
    licenceValidityPeriod: LicenceValidityPeriod,
    taxSituation: TaxSituation,
    saIncomeDeclared: Option[IncomeDeclared],
    entityType: Option[EntityType]
  ) extends UserAnswers

  object IncompleteUserAnswers {

    def fromCompleteAnswers(c: CompleteUserAnswers): IncompleteUserAnswers =
      IncompleteUserAnswers(
        Some(c.licenceType),
        Some(c.licenceTimeTrading),
        Some(c.licenceValidityPeriod),
        Some(c.taxSituation),
        c.saIncomeDeclared,
        c.entityType
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

  val empty: IncompleteUserAnswers = IncompleteUserAnswers(None, None, None, None, None, None)

  implicit val formatIncomplete: OFormat[IncompleteUserAnswers] = Json.format[IncompleteUserAnswers]
  implicit val formatComplete: OFormat[CompleteUserAnswers]     = Json.format[CompleteUserAnswers]

  @SuppressWarnings(Array("org.wartremover.warts.Throw", "org.wartremover.warts.Equals"))
  implicit val format: OFormat[UserAnswers] = Jsonx.oFormatSealed[UserAnswers]

  def taxSituation(userAnswers: UserAnswers): Option[TaxSituation] =
    userAnswers match {
      case i: IncompleteUserAnswers => i.taxSituation
      case c: CompleteUserAnswers   => Some(c.taxSituation)
    }

}
