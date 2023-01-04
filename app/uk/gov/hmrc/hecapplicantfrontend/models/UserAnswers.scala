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

import ai.x.play.json.Jsonx
import monocle.Lens
import monocle.macros.Lenses
import play.api.libs.json._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR}
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
sealed trait IndividualUserAnswers extends UserAnswers
sealed trait CompanyUserAnswers extends UserAnswers

sealed trait IncompleteUserAnswers extends UserAnswers
sealed trait CompleteUserAnswers extends UserAnswers

object IndividualUserAnswers {

  @Lenses
  final case class IncompleteIndividualUserAnswers(
    licenceType: Option[LicenceType],
    licenceTimeTrading: Option[LicenceTimeTrading],
    licenceValidityPeriod: Option[LicenceValidityPeriod],
    taxSituation: Option[TaxSituation],
    saIncomeDeclared: Option[YesNoAnswer],
    entityType: Option[EntityType]
  ) extends IndividualUserAnswers
      with IncompleteUserAnswers {
    val userAnswersType: UserAnswersType = UserAnswersType.Incomplete
  }

  final case class CompleteIndividualUserAnswers(
    licenceType: LicenceType,
    licenceTimeTrading: LicenceTimeTrading,
    licenceValidityPeriod: LicenceValidityPeriod,
    taxSituation: TaxSituation,
    saIncomeDeclared: Option[YesNoAnswer],
    entityType: Option[EntityType]
  ) extends IndividualUserAnswers
      with CompleteUserAnswers {
    val userAnswersType: UserAnswersType = UserAnswersType.Complete
  }

  object IncompleteIndividualUserAnswers {

    def fromCompleteAnswers(c: CompleteIndividualUserAnswers): IncompleteIndividualUserAnswers =
      IncompleteIndividualUserAnswers(
        Some(c.licenceType),
        Some(c.licenceTimeTrading),
        Some(c.licenceValidityPeriod),
        Some(c.taxSituation),
        c.saIncomeDeclared,
        c.entityType
      )

  }

  implicit class IndividualUserAnswersOps(private val u: IndividualUserAnswers) extends AnyVal {

    def fold[A](ifIncomplete: IncompleteIndividualUserAnswers => A, ifComplete: CompleteIndividualUserAnswers => A): A =
      u match {
        case i: IncompleteIndividualUserAnswers => ifIncomplete(i)
        case c: CompleteIndividualUserAnswers   => ifComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteIndividualUserAnswers.type => Lens[
        IncompleteIndividualUserAnswers,
        Option[A]
      ]
    ): IncompleteIndividualUserAnswers =
      fieldLens(IncompleteIndividualUserAnswers)
        .set(None)(fold(identity, IncompleteIndividualUserAnswers.fromCompleteAnswers))
  }

  val empty: IncompleteIndividualUserAnswers =
    IncompleteIndividualUserAnswers(None, None, None, None, None, None)

  implicit val format: OFormat[IndividualUserAnswers] = new OFormat[IndividualUserAnswers] {
    override def reads(json: JsValue): JsResult[IndividualUserAnswers] =
      (json \ "type")
        .validate[UserAnswersType]
        .flatMap {
          case UserAnswersType.Incomplete => Json.reads[IncompleteIndividualUserAnswers].reads(json)
          case UserAnswersType.Complete   => Json.reads[CompleteIndividualUserAnswers].reads(json)
        }

    override def writes(o: IndividualUserAnswers): JsObject = {
      val json = o match {
        case i: IncompleteIndividualUserAnswers => Json.writes[IncompleteIndividualUserAnswers].writes(i)
        case c: CompleteIndividualUserAnswers   => Json.writes[CompleteIndividualUserAnswers].writes(c)
      }
      json ++ Json.obj("type" -> o.userAnswersType)
    }
  }

}

object CompanyUserAnswers {

  @Lenses
  final case class IncompleteCompanyUserAnswers(
    licenceType: Option[LicenceType],
    licenceTimeTrading: Option[LicenceTimeTrading],
    licenceValidityPeriod: Option[LicenceValidityPeriod],
    entityType: Option[EntityType],
    crn: Option[CRN],
    companyDetailsConfirmed: Option[YesNoAnswer],
    chargeableForCT: Option[YesNoAnswer],
    ctIncomeDeclared: Option[YesNoAnswer],
    recentlyStartedTrading: Option[YesNoAnswer],
    ctutr: Option[CTUTR]
  ) extends CompanyUserAnswers
      with IncompleteUserAnswers {
    val userAnswersType: UserAnswersType = UserAnswersType.Incomplete
  }

  final case class CompleteCompanyUserAnswers(
    licenceType: LicenceType,
    licenceTimeTrading: LicenceTimeTrading,
    licenceValidityPeriod: LicenceValidityPeriod,
    entityType: Option[EntityType],
    crn: CRN,
    companyDetailsConfirmed: YesNoAnswer,
    chargeableForCT: Option[YesNoAnswer],
    ctIncomeDeclared: Option[YesNoAnswer],
    recentlyStartedTrading: Option[YesNoAnswer],
    ctutr: Option[CTUTR]
  ) extends CompanyUserAnswers
      with CompleteUserAnswers {
    val userAnswersType: UserAnswersType = UserAnswersType.Complete
  }

  object IncompleteCompanyUserAnswers {

    def fromCompleteAnswers(c: CompleteCompanyUserAnswers): IncompleteCompanyUserAnswers =
      IncompleteCompanyUserAnswers(
        Some(c.licenceType),
        Some(c.licenceTimeTrading),
        Some(c.licenceValidityPeriod),
        c.entityType,
        Some(c.crn),
        Some(c.companyDetailsConfirmed),
        c.chargeableForCT,
        c.ctIncomeDeclared,
        c.recentlyStartedTrading,
        c.ctutr
      )

  }

  implicit class CompanyUserAnswersOps(private val u: CompanyUserAnswers) extends AnyVal {

    def fold[A](ifIncomplete: IncompleteCompanyUserAnswers => A, ifComplete: CompleteCompanyUserAnswers => A): A =
      u match {
        case i: IncompleteCompanyUserAnswers => ifIncomplete(i)
        case c: CompleteCompanyUserAnswers   => ifComplete(c)
      }

    def unset[A](
      fieldLens: IncompleteCompanyUserAnswers.type => Lens[
        IncompleteCompanyUserAnswers,
        Option[A]
      ]
    ): IncompleteCompanyUserAnswers =
      fieldLens(IncompleteCompanyUserAnswers)
        .set(None)(fold(identity, IncompleteCompanyUserAnswers.fromCompleteAnswers))

  }

  val empty: IncompleteCompanyUserAnswers =
    IncompleteCompanyUserAnswers(None, None, None, None, None, None, None, None, None, None)

  implicit val format: OFormat[CompanyUserAnswers] = new OFormat[CompanyUserAnswers] {
    override def reads(json: JsValue): JsResult[CompanyUserAnswers] =
      (json \ "type")
        .validate[UserAnswersType]
        .flatMap {
          case UserAnswersType.Incomplete => Json.reads[IncompleteCompanyUserAnswers].reads(json)
          case UserAnswersType.Complete   => Json.reads[CompleteCompanyUserAnswers].reads(json)
        }

    override def writes(o: CompanyUserAnswers): JsObject = {
      val json = o match {
        case i: IncompleteCompanyUserAnswers => Json.writes[IncompleteCompanyUserAnswers].writes(i)
        case c: CompleteCompanyUserAnswers   => Json.writes[CompleteCompanyUserAnswers].writes(c)
      }
      json ++ Json.obj("type" -> o.userAnswersType)
    }
  }

}

object UserAnswers {
  implicit class UserAnswersOps(private val u: UserAnswers) extends AnyVal {

    def fold[A](
      ifIndividual: IndividualUserAnswers => A,
      ifCompany: CompanyUserAnswers => A
    ): A =
      u match {
        case i: IndividualUserAnswers => ifIndividual(i)
        case i: CompanyUserAnswers    => ifCompany(i)
      }

    def foldByCompleteness[A](
      ifIncomplete: IncompleteUserAnswers => A,
      ifComplete: CompleteUserAnswers => A
    ): A =
      u match {
        case i: IncompleteUserAnswers => ifIncomplete(i)
        case i: CompleteUserAnswers   => ifComplete(i)
      }
  }
}

object CompleteUserAnswers {
  import IndividualUserAnswers._
  import CompanyUserAnswers._

  implicit class CompleteUserAnswersOps(private val c: CompleteUserAnswers) extends AnyVal {
    def foldOnEntityType[A](
      ifIndividual: CompleteIndividualUserAnswers => A,
      ifCompany: CompleteCompanyUserAnswers => A
    ): A = c match {
      case individual: CompleteIndividualUserAnswers => ifIndividual(individual)
      case company: CompleteCompanyUserAnswers       => ifCompany(company)
    }

  }
}
