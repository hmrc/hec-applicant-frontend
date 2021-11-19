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
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, HECTaxCheckCode, HECTaxCheckSource}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.{Journey, LoginData, SaveTaxCheckRequest}
import uk.gov.hmrc.hecapplicantfrontend.testonly.util.UUIDGenerator
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

import java.time.LocalDate

@ImplementedBy(classOf[JourneyToLoginDataTransformerImpl])
trait JourneyToLoginDataTransformer {

  def toLoginData(journey: Journey): LoginData

}

@Singleton
class JourneyToLoginDataTransformerImpl @Inject() (uuidGenerator: UUIDGenerator, appConfig: AppConfig)
    extends JourneyToLoginDataTransformer {
  private val redirectUrl = s"${appConfig.selfBaseUrl}${routes.StartController.start().url}"

  private def newGGCredId(): GGCredId =
    GGCredId(uuidGenerator.randomUUID().toString)

  private def newExistingTaxCheck(ggCredId: GGCredId, verifier: Either[CRN, DateOfBirth]) =
    SaveTaxCheckRequest(
      HECTaxCheckCode("MK23TRDD9"),
      ggCredId,
      LicenceType.DriverOfTaxisAndPrivateHires,
      verifier,
      TimeUtils.today().plusDays(10L),
      TimeUtils.now(),
      TimeUtils.now(),
      isExtracted = true,
      HECTaxCheckSource.Digital
    )

  private def ctEnrolment(ctutr: CTUTR): Enrolment =
    Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", ctutr.value)), "Activated")

  private def saEnrolment(sautr: SAUTR): Enrolment =
    Enrolment("IR-SA", Seq(EnrolmentIdentifier("UTR", sautr.value)), "Activated")

  private val individualNoSA =
    LoginData(
      newGGCredId(),
      redirectUrl,
      ConfidenceLevel.L250,
      AffinityGroup.Individual,
      EmailAddress("user@test.com"),
      Some(NINO("NS123456C")),
      None,
      List.empty
    )

  private val companyNoCTEnrolment =
    LoginData(
      newGGCredId(),
      redirectUrl,
      ConfidenceLevel.L50,
      AffinityGroup.Organisation,
      EmailAddress("user@test.com"),
      None,
      None,
      List.empty
    )

  def toLoginData(journey: Journey): LoginData = journey match {
    case Journey.IndividualNoSA                          =>
      individualNoSA
    case Journey.IndividualSAReturnFound                 =>
      individualNoSA.copy(enrolment = Some(saEnrolment(SAUTR("1234567895"))))
    case Journey.IndividualSANoticeToFileIssued          =>
      individualNoSA.copy(enrolment = Some(saEnrolment(SAUTR("2222222222"))))
    case Journey.IndividualSANoReturnFound               =>
      individualNoSA.copy(enrolment = Some(saEnrolment(SAUTR("1111111111"))))
    case Journey.IndividualSAReturnFoundExistingTaxCheck =>
      val ggCredId         = newGGCredId()
      val existingTaxCheck = newExistingTaxCheck(
        ggCredId,
        Right(DateOfBirth(LocalDate.of(2000, 1, 1)))
      )

      individualNoSA.copy(
        ggCredId = ggCredId,
        enrolment = Some(saEnrolment(SAUTR("1234567895"))),
        existingTaxChecks = List(existingTaxCheck)
      )

    case Journey.CompanyNoCTEnrolment =>
      companyNoCTEnrolment

    case Journey.CompanyCTReturnFound =>
      companyNoCTEnrolment.copy(enrolment = Some(ctEnrolment(CTUTR("1111111111"))))

    case Journey.CompanyCTNoticeToFileIssued =>
      companyNoCTEnrolment.copy(enrolment = Some(ctEnrolment(CTUTR("2222222222"))))

    case Journey.CompanyCTNoReturnFound =>
      companyNoCTEnrolment.copy(enrolment = Some(ctEnrolment(CTUTR("3333333333"))))

    case Journey.CompanyCTNoAccountingPeriods =>
      companyNoCTEnrolment.copy(enrolment = Some(ctEnrolment(CTUTR("4444444444"))))

  }

}
