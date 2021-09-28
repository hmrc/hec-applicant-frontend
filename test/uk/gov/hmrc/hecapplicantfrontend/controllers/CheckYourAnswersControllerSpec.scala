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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.EitherT
import cats.instances.future._
import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.CheckYourAnswersControllerSpec.CheckYourAnswersRow
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.http.HeaderCarrier

import collection.JavaConverters._
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class CheckYourAnswersControllerSpec
    extends ControllerSpec
    with JourneyServiceSupport
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  val mockTaxCheckService = mock[TaxCheckService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TaxCheckService].toInstance(mockTaxCheckService)
  )

  val controller = instanceOf[CheckYourAnswersController]

  val individualRetrievedData =
    IndividualRetrievedData(
      GGCredId(""),
      NINO(""),
      None,
      Name("", ""),
      DateOfBirth(LocalDate.now()),
      None,
      None,
      List.empty
    )

  val companyRetrievedData = CompanyRetrievedData(GGCredId(""), None, None, None, List.empty)

  def mockSaveTaxCheck(applicantData: RetrievedApplicantData, completeAnswers: CompleteUserAnswers)(
    result: Either[Error, HECTaxCheck]
  ) =
    (mockTaxCheckService
      .saveTaxCheck(_: RetrievedApplicantData, _: CompleteUserAnswers)(_: HeaderCarrier))
      .expects(applicantData, completeAnswers, *)
      .returning(EitherT.fromEither(result))

  "CheckYourAnswersController" when {

    "handling requests to display the check your answers page" must {

      def performAction() = controller.checkYourAnswers(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "show an error page" when {

        "there are no complete answers in session" in {
          val session = HECSession(individualRetrievedData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "display the page" in {
        val answers = CompleteUserAnswers(
          LicenceType.ScrapMetalMobileCollector,
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToTwoYears,
          Some(TaxSituation.PAYE),
          Some(IncomeDeclared.Yes),
          Some(EntityType.Individual),
          None
        )

        val session = HECSession(individualRetrievedData, answers, None)

        val expectedRows =
          List(
            CheckYourAnswersRow(
              messageFromMessageKey("licenceType.title"),
              messageFromMessageKey("licenceType.scrapMetalCollector"),
              routes.LicenceDetailsController.licenceType().url
            ),
            CheckYourAnswersRow(
              messageFromMessageKey("licenceTimeTrading.title"),
              messageFromMessageKey("licenceTimeTrading.zeroToTwoYears"),
              routes.LicenceDetailsController.licenceTimeTrading().url
            ),
            CheckYourAnswersRow(
              messageFromMessageKey("licenceValidityPeriod.title"),
              messageFromMessageKey("licenceValidityPeriod.upToTwoYears"),
              routes.LicenceDetailsController.recentLicenceLength().url
            ),
            CheckYourAnswersRow(
              messageFromMessageKey("entityType.title"),
              messageFromMessageKey("entityType.individual"),
              routes.EntityTypeController.entityType().url
            ),
            CheckYourAnswersRow(
              messageFromMessageKey("taxSituation.title"),
              messageFromMessageKey("taxSituation.PA"),
              routes.TaxSituationController.taxSituation().url
            ),
            CheckYourAnswersRow(
              messageFromMessageKey("saIncomeDeclared.title"),
              messageFromMessageKey("saIncomeDeclared.yes"),
              routes.SAController.saIncomeStatement().url
            )
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.CheckYourAnswersController.checkYourAnswers(), session)(mockPreviousCall)
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("checkYourAnswers.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            val rows =
              doc.select(".govuk-summary-list__row").iterator().asScala.toList.map { element =>
                val question  = element.select(".govuk-summary-list__key").text()
                val answer    = element.select(".govuk-summary-list__value").text()
                val changeUrl = element.select(".govuk-link").attr("href")
                CheckYourAnswersRow(question, answer, changeUrl)
              }

            rows shouldBe expectedRows
          }
        )

      }

    }

    "handling submits on the check your answers page" must {

      def performAction() = controller.checkYourAnswersSubmit(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      val completeAnswers = CompleteUserAnswers(
        LicenceType.OperatorOfPrivateHireVehicles,
        LicenceTimeTrading.TwoToFourYears,
        LicenceValidityPeriod.UpToOneYear,
        Some(TaxSituation.SA),
        Some(IncomeDeclared.Yes),
        Some(EntityType.Individual),
        None
      )

      val session = HECSession(individualRetrievedData, completeAnswers, None)

      val hecTaxCheck = HECTaxCheck(HECTaxCheckCode(""), LocalDate.now())

      val updatedSession = HECSession(individualRetrievedData, UserAnswers.empty, Some(hecTaxCheck))

      "return an InternalServerError" when {

        "there are no complete answers in session" in {
          val session = HECSession(individualRetrievedData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "there is an error saving the tax check" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session.retrievedUserData, completeAnswers)(Left(Error(new Exception("Oh no!"))))
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR

        }

        "there is an error updating the session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session.retrievedUserData, completeAnswers)(Right(hecTaxCheck))
            mockJourneyServiceUpdateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers(),
              session,
              updatedSession
            )(Left(Error("")))
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "the tax check has successfully been saved" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSaveTaxCheck(session.retrievedUserData, completeAnswers)(Right(hecTaxCheck))
            mockJourneyServiceUpdateAndNext(
              routes.CheckYourAnswersController.checkYourAnswers(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction(), mockNextCall)
        }

      }

    }

  }

}

object CheckYourAnswersControllerSpec {

  final case class CheckYourAnswersRow(question: String, answer: String, changeUrl: String)

}
