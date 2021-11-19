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

package uk.gov.hmrc.hecapplicantfrontend.testonly.services

import com.google.inject.{ImplementedBy, Inject, Singleton}
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, HECTaxCheckCode, HECTaxCheckSource}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.Journey._
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.{Journey, LoginData, SaveTaxCheckRequest}
import uk.gov.hmrc.hecapplicantfrontend.testonly.util.UUIDGenerator
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider

import java.time.LocalDate

@ImplementedBy(classOf[JourneyToLoginDataTransformerImpl])
trait JourneyToLoginDataTransformer {

  def toLoginData(journey: Journey, redirectUrl: String): LoginData

}

@Singleton
class JourneyToLoginDataTransformerImpl @Inject() (uuidGenerator: UUIDGenerator, timeProvider: TimeProvider)
    extends JourneyToLoginDataTransformer {

  private def newGGCredId(): GGCredId =
    GGCredId(uuidGenerator.randomUUID().toString)

  private def newExistingTaxCheck(ggCredId: GGCredId, verifier: Either[CRN, DateOfBirth]) =
    SaveTaxCheckRequest(
      HECTaxCheckCode("MK23TRDD9"),
      ggCredId,
      LicenceType.DriverOfTaxisAndPrivateHires,
      verifier,
      timeProvider.currentDate.plusDays(10L),
      timeProvider.now,
      timeProvider.now,
      isExtracted = true,
      HECTaxCheckSource.Digital
    )

  private def ctEnrolment(ctutr: CTUTR): Enrolment =
    Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", ctutr.value)), "Activated")

  private def saEnrolment(sautr: SAUTR): Enrolment =
    Enrolment("IR-SA", Seq(EnrolmentIdentifier("UTR", sautr.value)), "Activated")

  private def individualNoSA(ggCredId: GGCredId, redirectUrl: String) =
    LoginData(
      ggCredId,
      redirectUrl,
      ConfidenceLevel.L250,
      AffinityGroup.Individual,
      EmailAddress("user@test.com"),
      Some(NINO("NS123456C")),
      None,
      List.empty
    )

  private def companyNoCTEnrolment(ggCredId: GGCredId, redirectUrl: String) =
    LoginData(
      ggCredId,
      redirectUrl,
      ConfidenceLevel.L50,
      AffinityGroup.Organisation,
      EmailAddress("user@test.com"),
      None,
      None,
      List.empty
    )

  def toLoginData(journey: Journey, redirectUrl: String): LoginData = journey match {
    case IndividualNoSA =>
      individualNoSA(newGGCredId(), redirectUrl)

    case IndividualSAReturnFound =>
      individualNoSA(newGGCredId(), redirectUrl).copy(enrolment = Some(saEnrolment(SAUTR("1234567895"))))

    case IndividualSANoticeToFileIssued =>
      individualNoSA(newGGCredId(), redirectUrl).copy(enrolment = Some(saEnrolment(SAUTR("2222222222"))))

    case IndividualSANoReturnFound =>
      individualNoSA(newGGCredId(), redirectUrl).copy(enrolment = Some(saEnrolment(SAUTR("1111111111"))))

    case IndividualSAReturnFoundExistingTaxCheck =>
      val ggCredId         = newGGCredId()
      val existingTaxCheck = newExistingTaxCheck(
        ggCredId,
        Right(DateOfBirth(LocalDate.of(2000, 1, 1)))
      )

      individualNoSA(ggCredId, redirectUrl).copy(
        enrolment = Some(saEnrolment(SAUTR("1234567895"))),
        existingTaxChecks = List(existingTaxCheck)
      )

    case CompanyNoCTEnrolment =>
      companyNoCTEnrolment(newGGCredId(), redirectUrl)

    case CompanyCTReturnFound =>
      companyNoCTEnrolment(newGGCredId(), redirectUrl).copy(enrolment = Some(ctEnrolment(CTUTR("1111111111"))))

    case CompanyCTNoticeToFileIssued =>
      companyNoCTEnrolment(newGGCredId(), redirectUrl).copy(enrolment = Some(ctEnrolment(CTUTR("2222222222"))))

    case CompanyCTNoReturnFound =>
      companyNoCTEnrolment(newGGCredId(), redirectUrl).copy(enrolment = Some(ctEnrolment(CTUTR("3333333333"))))

    case CompanyCTNoAccountingPeriods =>
      companyNoCTEnrolment(newGGCredId(), redirectUrl).copy(enrolment = Some(ctEnrolment(CTUTR("4444444444"))))

  }

}
