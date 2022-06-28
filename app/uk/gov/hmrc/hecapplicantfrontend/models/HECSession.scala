/*
 * Copyright 2022 HM Revenue & Customs
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
import cats.implicits.catsSyntaxEq
import monocle.Lens
import play.api.libs.json.{JsObject, JsResult, JsValue, Json, OFormat}
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.PasscodeRequestResult.PasscodeSent
import uk.gov.hmrc.hecapplicantfrontend.models.emailVerification.{PasscodeRequestResult, PasscodeVerificationResult}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState

import java.time.ZonedDateTime

trait HECSession extends Product with Serializable {
  val entityType: EntityType
  val userAnswers: UserAnswers
  val loginData: LoginData
  val retrievedJourneyData: RetrievedJourneyData
  val completedTaxCheck: Option[HECTaxCheck]
  val taxCheckStartDateTime: Option[ZonedDateTime]
  val unexpiredTaxChecks: List[TaxCheckListItem]
  val emailRequestedForTaxCheck: Option[EmailRequestedForTaxCheck]
  val hasResentEmailConfirmation: Boolean //flag added to separate the resend email confirmation journey
  val userEmailAnswers: Option[UserEmailAnswers]
  val showUserResearchBanner: Option[Boolean]
}

object HECSession {

  final case class IndividualHECSession(
    loginData: IndividualLoginData,
    retrievedJourneyData: IndividualRetrievedJourneyData,
    userAnswers: IndividualUserAnswers,
    completedTaxCheck: Option[HECTaxCheck],
    taxCheckStartDateTime: Option[ZonedDateTime],
    unexpiredTaxChecks: List[TaxCheckListItem],
    hasConfirmedDetails: Boolean,
    relevantIncomeTaxYear: Option[TaxYear],
    emailRequestedForTaxCheck: Option[EmailRequestedForTaxCheck],
    hasResentEmailConfirmation: Boolean,
    userEmailAnswers: Option[UserEmailAnswers],
    showUserResearchBanner: Option[Boolean]
  ) extends HECSession {
    override val entityType: EntityType = EntityType.Individual
  }

  object IndividualHECSession {

    def newSession(loginData: IndividualLoginData): IndividualHECSession =
      IndividualHECSession(
        loginData,
        IndividualRetrievedJourneyData.empty,
        IndividualUserAnswers.empty,
        None,
        None,
        List.empty,
        false,
        None,
        None,
        false,
        None,
        Some(true)
      )

  }

  final case class CompanyHECSession(
    loginData: CompanyLoginData,
    retrievedJourneyData: CompanyRetrievedJourneyData,
    userAnswers: CompanyUserAnswers,
    completedTaxCheck: Option[HECTaxCheck],
    taxCheckStartDateTime: Option[ZonedDateTime],
    unexpiredTaxChecks: List[TaxCheckListItem],
    crnBlocked: Boolean = false,
    emailRequestedForTaxCheck: Option[EmailRequestedForTaxCheck],
    hasResentEmailConfirmation: Boolean,
    userEmailAnswers: Option[UserEmailAnswers],
    showUserResearchBanner: Option[Boolean]
  ) extends HECSession {
    override val entityType: EntityType = EntityType.Company
  }

  object CompanyHECSession {

    def newSession(loginData: CompanyLoginData): CompanyHECSession =
      CompanyHECSession(
        loginData,
        CompanyRetrievedJourneyData.empty,
        CompanyUserAnswers.empty,
        None,
        None,
        List.empty,
        emailRequestedForTaxCheck = None,
        hasResentEmailConfirmation = false,
        userEmailAnswers = None,
        showUserResearchBanner = Some(true)
      )

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
        _ => InconsistentSessionState("Expected individual session data but got company session data").doThrow
      )

    def mapAsCompany[A](f: CompanyHECSession => A): A =
      s.fold(
        _ => InconsistentSessionState("Expected company session data but got individual session data").doThrow,
        f
      )

    /**
      * Replaces a field value in the session
      * @param session The session
      * @param individualLens Lens for individual incomplete user answers
      * @param companyLens Lens for company incomplete user answers
      * @param individualUpdate Update method for individual incomplete user answers
      * @param companyUpdate Update method for company incomplete user answers
      * @tparam A Represents the field value type of the field being replaced
      * @return The updated session
      */
    def replaceField[A](
      session: HECSession,
      individualLens: Lens[IncompleteIndividualUserAnswers, Option[A]],
      companyLens: Lens[IncompleteCompanyUserAnswers, Option[A]],
      individualUpdate: IncompleteIndividualUserAnswers => IncompleteIndividualUserAnswers,
      companyUpdate: IncompleteCompanyUserAnswers => IncompleteCompanyUserAnswers
    ): HECSession = session.fold(
      { individual =>
        val answers = individual.userAnswers.fold(
          i => individualUpdate(i.unset(_ => individualLens)),
          i => individualUpdate(i.unset(_ => individualLens))
        )
        individual.copy(userAnswers = answers)
      },
      { company =>
        val answers = company.userAnswers.fold(
          i => companyUpdate(i.unset(_ => companyLens)),
          i => companyUpdate(i.unset(_ => companyLens))
        )
        company.copy(userAnswers = answers)
      }
    )

    def ensureLicenceTypePresent[A](f: LicenceType => A): A = {
      import cats.implicits.catsSyntaxOptionId

      val licenceTypeOpt = s match {
        case i: IndividualHECSession => i.userAnswers.fold(_.licenceType, _.licenceType.some)
        case c: CompanyHECSession    => c.userAnswers.fold(_.licenceType, _.licenceType.some)
      }

      licenceTypeOpt match {
        case Some(licenceType) => f(licenceType)
        case None              => InconsistentSessionState("Couldn't find licence type").doThrow
      }
    }

    def isEmailRequested: Boolean = s.fold(_.emailRequestedForTaxCheck, _.emailRequestedForTaxCheck).isDefined

    def ensureEmailHasBeenRequested[A](f: EmailRequestedForTaxCheck => A): A =
      s.fold(_.emailRequestedForTaxCheck, _.emailRequestedForTaxCheck) match {
        case Some(e) => f(e)
        case None    => InconsistentSessionState("Email has not been requested").doThrow
      }

    def ensureGGEmailIdPresent[A](f: EmailAddress => A): A =
      s.fold(_.loginData.emailAddress, _.loginData.emailAddress) match {
        case Some(email) => f(email)
        case None        => InconsistentSessionState("No Email Address found in user's login session").doThrow
      }

    def ensureUserSelectedEmailPresent[A](f: UserSelectedEmail => A): A = s.userEmailAnswers
      .map(_.userSelectedEmail) match {
      case Some(userSelectedEmail) => f(userSelectedEmail)
      case None                    => InconsistentSessionState("No user selected email id in session").doThrow
    }

    def verifyPasscodeVerificationResultAndPasscodeRequestResult[A](f: => A): A =
      (
        s.userEmailAnswers.flatMap(_.passcodeVerificationResult),
        s.userEmailAnswers.flatMap(_.passcodeRequestResult)
      ) match {
        case (Some(pvr), Some(PasscodeSent)) if pvr === PasscodeVerificationResult.Match    => f
        case (None, Some(prr)) if prr === PasscodeRequestResult.EmailAddressAlreadyVerified => f
        case (other, otherPrr)                                                              =>
          InconsistentSessionState(
            s"Passcode verification result 'Match' is expected but got '$other', Passcode Request Result is '$otherPrr' "
          ).doThrow
      }

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
