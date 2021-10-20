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

package uk.gov.hmrc.hecapplicantfrontend.utils

import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.{CompanyRetrievedJourneyData, IndividualRetrievedJourneyData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids._
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models._

import java.time.{LocalDate, ZonedDateTime}

object Fixtures {

  def incompleteUserAnswers(
    licenceType: Option[LicenceType] = None,
    licenceTimeTrading: Option[LicenceTimeTrading] = None,
    licenceValidityPeriod: Option[LicenceValidityPeriod] = None,
    taxSituation: Option[TaxSituation] = None,
    saIncomeDeclared: Option[YesNoAnswer] = None,
    entityType: Option[EntityType] = None,
    crn: Option[CRN] = None,
    companyDetailsConfirmed: Option[YesNoAnswer] = None,
    chargeableForCT: Option[YesNoAnswer] = None,
    ctIncomeDeclared: Option[YesNoAnswer] = None,
    recentlyStartedTrading: Option[YesNoAnswer] = None,
    ctutr: Option[CTUTR] = None
  ): IncompleteUserAnswers = IncompleteUserAnswers(
    licenceType = licenceType,
    licenceTimeTrading = licenceTimeTrading,
    licenceValidityPeriod = licenceValidityPeriod,
    taxSituation = taxSituation,
    saIncomeDeclared = saIncomeDeclared,
    entityType = entityType,
    crn = crn,
    companyDetailsConfirmed = companyDetailsConfirmed,
    chargeableForCT = chargeableForCT,
    ctIncomeDeclared = ctIncomeDeclared,
    recentlyStartedTrading = recentlyStartedTrading,
    ctutr = ctutr
  )

  def completeUserAnswers(
    licenceType: LicenceType = LicenceType.DriverOfTaxisAndPrivateHires,
    licenceTimeTrading: LicenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
    licenceValidityPeriod: LicenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
    taxSituation: Option[TaxSituation] = None,
    saIncomeDeclared: Option[YesNoAnswer] = None,
    entityType: Option[EntityType] = None,
    crn: Option[CRN] = None,
    companyDetailsConfirmed: Option[YesNoAnswer] = None,
    chargeableForCT: Option[YesNoAnswer] = None,
    ctIncomeDeclared: Option[YesNoAnswer] = None,
    recentlyStartedTrading: Option[YesNoAnswer] = None,
    ctutr: Option[CTUTR] = None
  ): CompleteUserAnswers = CompleteUserAnswers(
    licenceType = licenceType,
    licenceTimeTrading = licenceTimeTrading,
    licenceValidityPeriod = licenceValidityPeriod,
    taxSituation = taxSituation,
    saIncomeDeclared = saIncomeDeclared,
    entityType = entityType,
    crn = crn,
    companyDetailsConfirmed = companyDetailsConfirmed,
    chargeableForCT = chargeableForCT,
    ctIncomeDeclared = ctIncomeDeclared,
    recentlyStartedTrading = recentlyStartedTrading,
    ctutr = ctutr
  )

  def individualLoginData(
    ggCredId: GGCredId = GGCredId("ggCredId"),
    nino: NINO = NINO("nino"),
    sautr: Option[SAUTR] = None,
    name: Name = Name("Sam", "Pull"),
    dateOfBirth: DateOfBirth = DateOfBirth(LocalDate.of(1990, 10, 10)),
    emailAddress: Option[EmailAddress] = None
  ): IndividualLoginData = IndividualLoginData(
    ggCredId = ggCredId,
    nino = nino,
    sautr = sautr,
    name = name,
    dateOfBirth = dateOfBirth,
    emailAddress = emailAddress
  )

  def individualRetrievedJourneyData(saStatus: Option[SAStatusResponse] = None): IndividualRetrievedJourneyData =
    IndividualRetrievedJourneyData(saStatus)

  def individualHECSession(
    loginData: IndividualLoginData = individualLoginData(),
    retrievedJourneyData: IndividualRetrievedJourneyData = individualRetrievedJourneyData(),
    userAnswers: UserAnswers = incompleteUserAnswers(),
    completedTaxCheck: Option[HECTaxCheck] = None,
    taxCheckStartDateTime: Option[ZonedDateTime] = None,
    unexpiredTaxChecks: List[TaxCheckListItem] = List.empty
  ): IndividualHECSession =
    IndividualHECSession(
      loginData = loginData,
      retrievedJourneyData = retrievedJourneyData,
      userAnswers = userAnswers,
      completedTaxCheck = completedTaxCheck,
      taxCheckStartDateTime = taxCheckStartDateTime,
      unexpiredTaxChecks = unexpiredTaxChecks
    )

  def companyLoginData(
    ggCredId: GGCredId = GGCredId("ggCredId"),
    ctutr: Option[CTUTR] = None,
    emailAddress: Option[EmailAddress] = None
  ): CompanyLoginData = CompanyLoginData(
    ggCredId = ggCredId,
    ctutr = ctutr,
    emailAddress = emailAddress
  )

  def companyRetrievedJourneyData(
    companyName: Option[CompanyHouseName] = None,
    desCtutr: Option[CTUTR] = None,
    ctStatus: Option[CTStatusResponse] = None
  ): CompanyRetrievedJourneyData = CompanyRetrievedJourneyData(
    companyName = companyName,
    desCtutr = desCtutr,
    ctStatus = ctStatus
  )

  def companyHECSession(
    loginData: CompanyLoginData = companyLoginData(),
    retrievedJourneyData: CompanyRetrievedJourneyData = companyRetrievedJourneyData(),
    userAnswers: UserAnswers = incompleteUserAnswers(),
    completedTaxCheck: Option[HECTaxCheck] = None,
    taxCheckStartDateTime: Option[ZonedDateTime] = None,
    unexpiredTaxChecks: List[TaxCheckListItem] = List.empty,
    ctutrAnswerAttempts: Int = 1
  ): CompanyHECSession = CompanyHECSession(
    loginData = loginData,
    retrievedJourneyData = retrievedJourneyData,
    userAnswers = userAnswers,
    completedTaxCheck = completedTaxCheck,
    taxCheckStartDateTime = taxCheckStartDateTime,
    unexpiredTaxChecks = unexpiredTaxChecks,
    ctutrAnswerAttempts = ctutrAnswerAttempts
  )

  def ctStatusResponse(
    ctutr: CTUTR = CTUTR("utr"),
    startDate: LocalDate = LocalDate.now,
    endDate: LocalDate = LocalDate.now,
    latestAccountingPeriod: Option[CTAccountingPeriod] = None
  ): CTStatusResponse = CTStatusResponse(
    ctutr = ctutr,
    startDate = startDate,
    endDate = endDate,
    latestAccountingPeriod = latestAccountingPeriod
  )

  def ctAccountingPeriod(
    startDate: LocalDate = LocalDate.now,
    endDate: LocalDate = LocalDate.now,
    ctStatus: CTStatus = CTStatus.ReturnFound
  ): CTAccountingPeriod = CTAccountingPeriod(
    startDate = startDate,
    endDate = endDate,
    ctStatus = ctStatus
  )

}
