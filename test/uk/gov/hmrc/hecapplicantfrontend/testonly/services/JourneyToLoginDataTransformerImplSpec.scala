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

package uk.gov.hmrc.hecapplicantfrontend.testonly.services

import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike
import uk.gov.hmrc.auth.core.AffinityGroup.{Individual, Organisation}
import uk.gov.hmrc.auth.core.ConfidenceLevel.{L250, L50}
import uk.gov.hmrc.auth.core.{Enrolment, EnrolmentIdentifier}
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.HECTaxCheckSource
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EmailAddress, HECTaxCheckCode}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.Journey._
import uk.gov.hmrc.hecapplicantfrontend.testonly.models.{LoginData, SaveTaxCheckRequest}
import uk.gov.hmrc.hecapplicantfrontend.testonly.util.UUIDGenerator
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider

import java.time.{LocalDate, ZonedDateTime}
import java.util.UUID

class JourneyToLoginDataTransformerImplSpec extends Matchers with AnyWordSpecLike with MockFactory {

  val mockUUIDGenerator = mock[UUIDGenerator]

  val mockTimeProvider = mock[TimeProvider]

  val transformer = new JourneyToLoginDataTransformerImpl(mockUUIDGenerator, mockTimeProvider)

  def mockGenerateUUID(uuid: UUID) =
    (mockUUIDGenerator.randomUUID _)
      .expects()
      .returning(uuid)

  def mockTimeProviderCurrentDate(date: LocalDate) =
    (mockTimeProvider.currentDate _)
      .expects()
      .returning(date)

  def mockTimeProviderNow(now: ZonedDateTime) =
    (mockTimeProvider.now _)
      .expects()
      .returning(now)

  "JourneyToLoginDataTransformerImpl" when {

    "handling requests to transform Journeys into LoginData" must {

      "transform correctly" when afterWord("passed a") {

        val uuid = UUID.randomUUID()

        val ggCredId = GGCredId(uuid.toString)

        val redirectUrl = "redirect"

        "IndividualNoSA" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(IndividualNoSA, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L250,
            Individual,
            EmailAddress("user@test.com"),
            Some(NINO("NS123456C")),
            None,
            List.empty
          )
        }

        "IndividualSAReturnFound" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(IndividualSAReturnFound, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L250,
            Individual,
            EmailAddress("user@test.com"),
            Some(NINO("NS123456C")),
            Some(Enrolment("IR-SA", Seq(EnrolmentIdentifier("UTR", "1234567895")), "Activated")),
            List.empty
          )
        }

        "IndividualSANoticeToFileIssued" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(IndividualSANoticeToFileIssued, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L250,
            Individual,
            EmailAddress("user@test.com"),
            Some(NINO("NS123456C")),
            Some(Enrolment("IR-SA", Seq(EnrolmentIdentifier("UTR", "2222222222")), "Activated")),
            List.empty
          )
        }

        "IndividualSANoReturnFound" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(IndividualSANoReturnFound, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L250,
            Individual,
            EmailAddress("user@test.com"),
            Some(NINO("NS123456C")),
            Some(Enrolment("IR-SA", Seq(EnrolmentIdentifier("UTR", "1111111111")), "Activated")),
            List.empty
          )
        }

        "IndividualSAReturnFoundExistingTaxCheck" in {
          val today                 = LocalDate.now()
          val createdAt             = ZonedDateTime.now()
          val taxCheckStartDateTime = ZonedDateTime.now

          inSequence {
            mockGenerateUUID(uuid)
            mockTimeProviderCurrentDate(today)
            mockTimeProviderNow(createdAt)
            mockTimeProviderNow(taxCheckStartDateTime)
          }

          transformer.toLoginData(IndividualSAReturnFoundExistingTaxCheck, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L250,
            Individual,
            EmailAddress("user@test.com"),
            Some(NINO("NS123456C")),
            Some(Enrolment("IR-SA", Seq(EnrolmentIdentifier("UTR", "1234567895")), "Activated")),
            List(
              SaveTaxCheckRequest(
                HECTaxCheckCode("MK23TRDD9"),
                ggCredId,
                LicenceType.DriverOfTaxisAndPrivateHires,
                Right(DateOfBirth(LocalDate.of(2000, 1, 1))),
                today.plusDays(10L),
                createdAt,
                taxCheckStartDateTime,
                isExtracted = true,
                HECTaxCheckSource.Digital
              )
            )
          )
        }

        "CompanyNoCTEnrolment" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(CompanyNoCTEnrolment, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L50,
            Organisation,
            EmailAddress("user@test.com"),
            None,
            None,
            List.empty
          )
        }

        "CompanyCTReturnFound" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(CompanyCTReturnFound, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L50,
            Organisation,
            EmailAddress("user@test.com"),
            None,
            Some(Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", "1111111111")), "Activated")),
            List.empty
          )
        }

        "CompanyCTNoticeToFileIssued" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(CompanyCTNoticeToFileIssued, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L50,
            Organisation,
            EmailAddress("user@test.com"),
            None,
            Some(Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", "2222222222")), "Activated")),
            List.empty
          )
        }

        "CompanyCTNoReturnFound" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(CompanyCTNoReturnFound, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L50,
            Organisation,
            EmailAddress("user@test.com"),
            None,
            Some(Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", "3333333333")), "Activated")),
            List.empty
          )
        }

        "CompanyCTNoAccountingPeriods" in {
          mockGenerateUUID(uuid)

          transformer.toLoginData(CompanyCTNoAccountingPeriods, redirectUrl) shouldBe LoginData(
            ggCredId,
            redirectUrl,
            L50,
            Organisation,
            EmailAddress("user@test.com"),
            None,
            Some(Enrolment("IR-CT", Seq(EnrolmentIdentifier("UTR", "4444444444")), "Activated")),
            List.empty
          )
        }

      }

    }

  }

}
