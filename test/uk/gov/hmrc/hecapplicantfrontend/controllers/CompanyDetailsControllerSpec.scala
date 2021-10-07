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
import play.api.http.Status.INTERNAL_SERVER_ERROR
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers.{defaultAwaitTimeout, status}
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyJourneyData, CompanyLoginData, CompanyRetrievedData, IndividualJourneyData, IndividualLoginData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CompanyDetailsControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider    = mock[TimeProvider]
  val mockTaxCheckService = mock[TaxCheckService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[TaxCheckService].toInstance(mockTaxCheckService)
  )

  def mockTimeProviderToday(d: LocalDate) = (mockTimeProvider.currentDate _).expects().returning(d)

  def mockTaxCheckServiceGetCtutr(crn: CRN)(result: Either[Error, Option[CTUTR]]) =
    (mockTaxCheckService
      .getCtutr(_: CRN)(_: HeaderCarrier))
      .expects(crn, *)
      .returning(EitherT.fromEither[Future](result))

  def mockTaxCheckServiceGetCtStatus(ctutr: CTUTR, startDate: LocalDate, endDate: LocalDate)(
    result: Either[Error, Option[CTStatusResponse]]
  ) =
    (mockTaxCheckService
      .getCTStatus(_: CTUTR, _: LocalDate, _: LocalDate)(_: HeaderCarrier))
      .expects(ctutr, startDate, endDate, *)
      .returning(EitherT.fromEither[Future](result))

  val controller           = instanceOf[CompanyDetailsController]
  val companyRetrievedData =
    CompanyRetrievedData(
      CompanyLoginData(GGCredId(""), None, None),
      CompanyJourneyData(Some(CompanyHouseName("some-company")), None, None),
      List.empty
    )

  "CompanyDetailsControllerSpec" when {

    "handling requests to the confirm company details page " must {

      def performAction(): Future[Result] = controller.confirmCompanyDetails(FakeRequest())

      "display the page" when {

        "the user has not previously answered the question " in {

          val session = HECSession(companyRetrievedData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.confirmCompanyDetailsSubmit().url
            }
          )

        }

        "the user has previously answered the question" in {

          val answers = CompleteUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear,
            None,
            None,
            None,
            None,
            Some(YesNoAnswer.Yes)
          )
          val session = HECSession(companyRetrievedData, answers, None)

          val updatedAnswers = IncompleteUserAnswers
            .fromCompleteAnswers(answers)
            .copy(companyDetailsConfirmed = Some(YesNoAnswer.No))
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              updatedSession
            )(mockPreviousCall)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "1"

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CompanyDetailsController.confirmCompanyDetailsSubmit().url
            }
          )

        }

      }

      "return internal server error" when {
        "company name is not populated" in {
          val session = HECSession(
            companyRetrievedData.copy(
              journeyData = companyRetrievedData.journeyData.copy(
                companyName = None
              )
            ),
            UserAnswers.empty,
            None
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "applicant is individual" in {
          val individualData = IndividualRetrievedData(
            IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None),
            IndividualJourneyData.empty,
            List.empty
          )
          val session        = HECSession(individualData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }
      }
    }

    "handling submit on the confirm company name page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.confirmCompanyDetailsSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val session = HECSession(companyRetrievedData, UserAnswers.empty.copy(crn = Some(CRN("crn"))), None)

        "nothing has been submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey("confirmCompanyName.error.required")
          )
        }

        "an invalid index value is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("confirmCompanyName" -> Int.MaxValue.toString),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey("confirmCompanyName.error.invalid")
          )
        }

        "a non-numeric value is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CompanyDetailsController.confirmCompanyDetails(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("confirmCompanyName" -> "xyz"),
            messageFromMessageKey("confirmCompanyName.title"),
            messageFromMessageKey("confirmCompanyName.error.invalid")
          )
        }
      }

      val date                 = LocalDate.now
      val (startDate, endDate) = (date.minusYears(2).plusDays(1), date.minusYears(1))

      "return an internal server error" when {

        "user answers with a Yes" when {

          "CRN is not populated" in {
            val answers = UserAnswers.empty
            // session contains CTUTR from enrolments
            val session = HECSession(
              companyRetrievedData.copy(
                loginData = companyRetrievedData.loginData.copy(
                  ctutr = Some(CTUTR("ctutr"))
                )
              ),
              answers,
              None
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
          }

          "the applicant type is individual" in {
            val individualData = IndividualRetrievedData(
              IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None),
              IndividualJourneyData.empty,
              List.empty
            )
            val answers        = UserAnswers.empty.copy(crn = Some(CRN("crn")))
            val session        = HECSession(individualData, answers, None)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
          }

          "the call to fetch CT status fails" in {
            val answers = UserAnswers.empty.copy(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val session = HECSession(
              companyRetrievedData.copy(loginData = companyRetrievedData.loginData.copy(ctutr = Some(CTUTR("ctutr")))),
              answers,
              None
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(CTUTR("ctutr"))))
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(CTUTR("ctutr"), startDate, endDate)(
                Left(Error("fetch ct status failed"))
              )
            }

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
          }

          "the call to update and next fails" in {
            val answers     = UserAnswers.empty.copy(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val companyData =
              companyRetrievedData.copy(loginData = companyRetrievedData.loginData.copy(ctutr = Some(CTUTR("ctutr"))))
            val session     = HECSession(companyData, answers, None)

            val updatedAnswers   = answers.copy(companyDetailsConfirmed = Some(YesNoAnswer.Yes))
            val ctStatusResponse = CTStatusResponse(CTUTR("ctutr"), date, date, None)
            val updatedSession   = session.copy(
              userAnswers = updatedAnswers,
              retrievedUserData = companyData.copy(
                journeyData = companyData.journeyData
                  .copy(desCtutr = companyData.loginData.ctutr, ctStatus = Some(ctStatusResponse))
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(CTUTR("ctutr"))))
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(CTUTR("ctutr"), startDate, endDate)(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(
                Left(Error("update and next failed"))
              )
            }

            status(performAction("confirmCompanyName" -> "0")) shouldBe INTERNAL_SERVER_ERROR
          }
        }

        "user answers with a No" when {
          "the call to update and next fails" in {
            val answers = UserAnswers.empty.copy(crn = Some(CRN("crn")))
            val session = HECSession(companyRetrievedData, answers, None)

            // should wipe out CRN answer if user says that the company name is incorrect
            val updatedAnswers = answers.copy(crn = None)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(Left(Error("some error")))
            }

            status(performAction("confirmCompanyName" -> "1")) shouldBe INTERNAL_SERVER_ERROR
          }
        }

      }

      "redirect to the next page" when {

        "user answers with a Yes and all data fetches are successful" when {
          "the enrolment and DES CTUTRs match" in {
            val answers     = UserAnswers.empty.copy(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val companyData = companyRetrievedData.copy(
              loginData = companyRetrievedData.loginData.copy(ctutr = Some(CTUTR("ctutr")))
            )
            val session     = HECSession(companyData, answers, None)

            val updatedAnswers   = answers.copy(companyDetailsConfirmed = Some(YesNoAnswer.Yes))
            val ctStatusResponse = CTStatusResponse(CTUTR("ctutr"), date, date, None)
            val updatedSession   = session.copy(
              userAnswers = updatedAnswers,
              retrievedUserData = companyData.copy(
                journeyData = companyData.journeyData
                  .copy(desCtutr = companyData.loginData.ctutr, ctStatus = Some(ctStatusResponse))
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(CTUTR("ctutr"))))
              mockTimeProviderToday(date)
              mockTaxCheckServiceGetCtStatus(CTUTR("ctutr"), startDate, endDate)(
                Right(Some(ctStatusResponse))
              )
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("confirmCompanyName" -> "0"), mockNextCall)
          }

          "the enrolment and DES CTUTRs do not match" in {
            val answers     = UserAnswers.empty.copy(crn = Some(CRN("crn")))
            // session contains CTUTR from enrolments
            val companyData = companyRetrievedData.copy(
              loginData = companyRetrievedData.loginData.copy(ctutr = Some(CTUTR("ctutr")))
            )
            val session     = HECSession(companyData, answers, None)

            val updatedAnswers = answers.copy(companyDetailsConfirmed = Some(YesNoAnswer.Yes))
            val desCtutr       = CTUTR("des-ctutr")
            val updatedSession = session.copy(
              userAnswers = updatedAnswers,
              retrievedUserData = companyData.copy(
                journeyData = companyData.journeyData.copy(desCtutr = Some(desCtutr))
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTaxCheckServiceGetCtutr(CRN("crn"))(Right(Some(desCtutr)))
              mockJourneyServiceUpdateAndNext(
                routes.CompanyDetailsController.confirmCompanyDetails(),
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction("confirmCompanyName" -> "0"), mockNextCall)
          }
        }

        "user answers with a No" in {
          val answers = UserAnswers.empty.copy(crn = Some(CRN("crn")))
          val session = HECSession(companyRetrievedData, answers, None)

          // should wipe out CRN answer if user says that the company name is incorrect
          val updatedAnswers = answers.copy(crn = None)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.CompanyDetailsController.confirmCompanyDetails(),
              session,
              updatedSession
            )(Right(mockNextCall))
          }

          checkIsRedirect(performAction("confirmCompanyName" -> "1"), mockNextCall)
        }

      }

    }

  }

}
