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

import julienrf.json.derived
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json.OFormat

sealed trait UserAnswers extends Product with Serializable

object UserAnswers {

  @Lenses
  final case class IncompleteUserAnswers(
    licenceType: Option[LicenceType] = None,
    licenceExpiryDate: Option[LicenceExpiryDate] = None
  ) extends UserAnswers

  final case class CompleteUserAnswers(licenceType: LicenceType, licenceExpiryDate: LicenceExpiryDate)
      extends UserAnswers

  object IncompleteUserAnswers {

    def fromCompleteAnswers(c: CompleteUserAnswers): IncompleteUserAnswers =
      IncompleteUserAnswers(Some(c.licenceType), Some(c.licenceExpiryDate))

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

  val empty: IncompleteUserAnswers = IncompleteUserAnswers(None, None)

  implicit val format: OFormat[UserAnswers] = derived.oformat()

}
