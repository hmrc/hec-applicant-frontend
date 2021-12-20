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
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.IncompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.CompanyLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.CompanyRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, CTUTR, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyHouseDetails, CompanyHouseName, CtutrAttempts, Error, YesNoAnswer}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{CompanyDetailsService, CtutrAttemptsService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

import java.time.ZonedDateTime
import java.util.Locale
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class CRNControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockCompanyDetailsService = mock[CompanyDetailsService]
  val validCRN                  = CRN("11123456")
  val validCRNs                 =
    List(validCRN, CRN("1S1 23 45"), CRN("1S123456"), CRN("1s123456"), CRN("1S12345"), CRN("1112345"))
  val nonAlphaNumCRN            = List(CRN("$£%^&"), CRN("AA1244&"))
  val inValidCRN                =
    List(CRN("AAB3456"), CRN("12345AAA"))
  val companyHouseName          = CompanyHouseName("Test Tech Ltd")

  val mockCtutrAttemptsService = mock[CtutrAttemptsService]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[CompanyDetailsService].toInstance(mockCompanyDetailsService),
    bind[CtutrAttemptsService].toInstance(mockCtutrAttemptsService)
  )

  val controller       = instanceOf[CRNController]
  val companyLoginData = CompanyLoginData(GGCredId(""), None, None)

  def mockFindCompany(crn: CRN)(
    result: Either[Error, Option[CompanyHouseDetails]]
  ) = (mockCompanyDetailsService
    .findCompany(_: CRN)(_: HeaderCarrier))
    .expects(crn, *)
    .returning(EitherT.fromEither(result))

  def mockCtutrAttemptsServiceGet(crn: CRN, ggCredId: GGCredId)(
    result: Either[Error, Option[CtutrAttempts]]
  ) =
    (mockCtutrAttemptsService
      .get(_: CRN, _: GGCredId))
      .expects(crn, ggCredId)
      .returning(EitherT.fromEither[Future](result))

  "CRNControllerSpec" when {

    "handling requests to the Company Registration Number page " must {

      def performAction(): Future[Result] = controller.companyRegistrationNumber(FakeRequest())

      "display the page" when {

        "the user has not previously answered the question " in {

          val session = CompanyHECSession.newSession(companyLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(mockPreviousCall)
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("crn.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CRNController.companyRegistrationNumberSubmit().url

              val link = doc.select("p > .govuk-link")
              link.text should startWith(messageFromMessageKey("crn.link"))

            }
          )

        }

        "the user has previously answered the question" in {

          val answers = Fixtures.completeCompanyUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear
          )

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(crn = Some(validCRN))
          val session        =
            Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(
              routes.CRNController.companyRegistrationNumber(),
              updatedSession
            )(mockPreviousCall)
          }
          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("crn.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val button = doc.select("form")
              button.attr("action") shouldBe routes.CRNController.companyRegistrationNumberSubmit().url

              val link = doc.select("p > .govuk-link")
              link.text should startWith(messageFromMessageKey("crn.link"))

              val input = doc.select(".govuk-input")
              input.attr("value") shouldBe validCRN.value

            }
          )

        }

      }

    }

    "handling submit on the CRN page" must {

      def performAction(data: (String, String)*): Future[Result] =
        controller.companyRegistrationNumberSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      val companyName = CompanyHouseName("test company")

      "show a form error" when {

        val session = CompanyHECSession.newSession(companyLoginData)

        "nothing has been submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("crn.title"),
            messageFromMessageKey("crn.error.required")
          )
        }

        "the submitted value is too long" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("crn" -> "1234567890"),
            messageFromMessageKey("crn.title"),
            messageFromMessageKey("crn.error.crnInvalid")
          )
        }

        "the submitted value is too short" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("crn" -> "12345"),
            messageFromMessageKey("crn.title"),
            messageFromMessageKey("crn.error.crnInvalid")
          )
        }

        "the submitted value contains characters which are not letters or digits" in {

          nonAlphaNumCRN.foreach { crn =>
            withClue(s"For CRN $crn: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(
                  mockPreviousCall
                )
              }

              checkFormErrorIsDisplayed(
                performAction("crn" -> crn.value),
                messageFromMessageKey("crn.title"),
                messageFromMessageKey("crn.error.nonAlphanumericChars")
              )

            }

          }

        }

        "the submitted value contains alphanumeric characters but in wrong format" in {
          inValidCRN.foreach { crn =>
            withClue(s"For CRN $crn: ") {
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(
                  mockPreviousCall
                )
              }

              checkFormErrorIsDisplayed(
                performAction("crn" -> crn.value),
                messageFromMessageKey("crn.title"),
                messageFromMessageKey("crn.error.crnInvalid")
              )

            }
          }
        }

        "CRN match was not found in companies house db" in {
          val crn           = validCRN
          val ctutrAttempts = CtutrAttempts(crn, GGCredId("ggCredId"), companyName, 1, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGet(crn, session.loginData.ggCredId)(Right(Some(ctutrAttempts)))
            mockFindCompany(validCRN)(Right(None))
            mockJourneyServiceGetPrevious(routes.CRNController.companyRegistrationNumber(), session)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("crn" -> crn.value),
            messageFromMessageKey("crn.title"),
            messageFromMessageKey("crn.error.notFoundInCompaniesHouse")
          )
        }

      }

      "return a technical error" when {

        val answers = Fixtures.completeCompanyUserAnswers()
        val session = Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)

        "session is for individual" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession())
          }

          assertThrows[RuntimeException](await(performAction("crn" -> validCRN.value)))
        }

        "there is an error updating and getting the next endpoint" in {
          val crn           = validCRN
          val ctutrAttempts = CtutrAttempts(crn, GGCredId("ggCredId"), companyName, 1, None)

          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(crn = Some(validCRN), companyDetailsConfirmed = None)

          val updatedSession = session.copy(
            retrievedJourneyData = session.retrievedJourneyData.copy(companyName = Some(companyName)),
            userAnswers = updatedAnswers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGet(crn, session.loginData.ggCredId)(Right(Some(ctutrAttempts)))
            mockFindCompany(validCRN)(
              Right(Some(CompanyHouseDetails(companyName)))
            )
            mockJourneyServiceUpdateAndNext(
              routes.CRNController.companyRegistrationNumber(),
              session,
              updatedSession
            )(
              Left(Error(""))
            )
          }

          assertThrows[RuntimeException](await(performAction("crn" -> crn.value)))

        }

        "CRN is blocked & updateAndNext fails" in {
          val crn                  = validCRN
          val answers              = Fixtures.completeCompanyUserAnswers()
          val session              = Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
          val blockedCtutrAttempts =
            CtutrAttempts(crn, session.loginData.ggCredId, companyName, 1, Some(ZonedDateTime.now))

          // CRN dependent answers are reset
          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(
              crn = Some(crn),
              companyDetailsConfirmed = None,
              chargeableForCT = None,
              ctIncomeDeclared = None,
              recentlyStartedTrading = None,
              ctutr = None
            )

          val updatedSession = session.copy(userAnswers = updatedAnswers, crnBlocked = true)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGet(crn, session.loginData.ggCredId)(Right(Some(blockedCtutrAttempts)))
            mockJourneyServiceUpdateAndNext(
              routes.CRNController.companyRegistrationNumber(),
              session,
              updatedSession
            )(Left(Error("some error")))
          }

          assertThrows[RuntimeException](await(performAction("crn" -> crn.value)))
        }

      }

      "redirect to the next page" when {

        "a blocked CRN is submitted" in {
          val crn                  = validCRN
          val answers              = Fixtures.completeCompanyUserAnswers()
          val session              = Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
          val blockedCtutrAttempts =
            CtutrAttempts(crn, session.loginData.ggCredId, companyName, 1, Some(ZonedDateTime.now))

          // CRN dependent answers are reset
          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(
              crn = Some(crn),
              companyDetailsConfirmed = None,
              chargeableForCT = None,
              ctIncomeDeclared = None,
              recentlyStartedTrading = None,
              ctutr = None
            )

          val updatedSession = session.copy(userAnswers = updatedAnswers, crnBlocked = true)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGet(crn, session.loginData.ggCredId)(Right(Some(blockedCtutrAttempts)))
            mockJourneyServiceUpdateAndNext(
              routes.CRNController.companyRegistrationNumber(),
              session,
              updatedSession
            )(
              Right(mockNextCall)
            )
          }
          checkIsRedirect(performAction("crn" -> crn.value), mockNextCall)
        }

        "a valid & unblocked CRN is submitted and company is found" in {
          val companyDetails = Some(CompanyHouseDetails(companyName))
          validCRNs.foreach { crn =>
            withClue(s"for CRN: $crn") {

              val formattedCrn  = CRN(crn.value.removeWhitespace.toUpperCase(Locale.UK))
              val answers       = Fixtures.completeCompanyUserAnswers()
              val session       = Fixtures.companyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers)
              val ctutrAttempts = CtutrAttempts(formattedCrn, session.loginData.ggCredId, companyName, 1, None)

              val updatedAnswers = IncompleteCompanyUserAnswers
                .fromCompleteAnswers(answers)
                .copy(crn = Some(formattedCrn), companyDetailsConfirmed = None)

              val updatedRetrievedJourneyData =
                session.retrievedJourneyData.copy(companyName = companyDetails.map(_.companyName))
              val updatedSession              =
                session.copy(retrievedJourneyData = updatedRetrievedJourneyData, userAnswers = updatedAnswers)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockCtutrAttemptsServiceGet(formattedCrn, session.loginData.ggCredId)(Right(Some(ctutrAttempts)))
                mockFindCompany(formattedCrn)(
                  Right(companyDetails)
                )
                mockJourneyServiceUpdateAndNext(
                  routes.CRNController.companyRegistrationNumber(),
                  session,
                  updatedSession
                )(
                  Right(mockNextCall)
                )
              }
              checkIsRedirect(performAction("crn" -> crn.value), mockNextCall)
            }
          }
        }

        "previously set CRN is changed, CRN dependent answers are reset" in {
          val companyDetails = Some(CompanyHouseDetails(companyName))

          val answers       = Fixtures.completeCompanyUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            crn = CRN("old-crn"),
            companyDetailsConfirmed = YesNoAnswer.Yes,
            chargeableForCT = Some(YesNoAnswer.Yes),
            ctIncomeDeclared = Some(YesNoAnswer.Yes),
            recentlyStartedTrading = Some(YesNoAnswer.No),
            ctutr = Some(CTUTR("some-ctutr"))
          )
          val session       = Fixtures.companyHECSession(loginData = companyLoginData, userAnswers = answers)
          val ctutrAttempts = CtutrAttempts(validCRN, session.loginData.ggCredId, companyName, 1, None)

          // CRN dependent answers are reset
          val updatedAnswers = IncompleteCompanyUserAnswers
            .fromCompleteAnswers(answers)
            .copy(
              crn = Some(validCRN),
              companyDetailsConfirmed = None,
              chargeableForCT = None,
              ctIncomeDeclared = None,
              recentlyStartedTrading = None,
              ctutr = None
            )

          val updatedRetrievedJourneyData =
            session.retrievedJourneyData.copy(companyName = companyDetails.map(_.companyName))
          val updatedSession              =
            session.copy(retrievedJourneyData = updatedRetrievedJourneyData, userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGet(validCRN, session.loginData.ggCredId)(Right(Some(ctutrAttempts)))
            mockFindCompany(validCRN)(
              Right(companyDetails)
            )
            mockJourneyServiceUpdateAndNext(
              routes.CRNController.companyRegistrationNumber(),
              session,
              updatedSession
            )(
              Right(mockNextCall)
            )
          }
          checkIsRedirect(performAction("crn" -> validCRN.value), mockNextCall)
        }

        "previously set CRN is unchanged, CRN dependent answers are not reset" in {
          val answers       = Fixtures.completeCompanyUserAnswers(
            licenceType = LicenceType.OperatorOfPrivateHireVehicles,
            licenceTimeTrading = LicenceTimeTrading.ZeroToTwoYears,
            licenceValidityPeriod = LicenceValidityPeriod.UpToOneYear,
            crn = validCRN,
            companyDetailsConfirmed = YesNoAnswer.Yes,
            chargeableForCT = Some(YesNoAnswer.Yes),
            ctIncomeDeclared = Some(YesNoAnswer.Yes),
            recentlyStartedTrading = Some(YesNoAnswer.No),
            ctutr = Some(CTUTR("some-ctutr"))
          )
          val session       =
            Fixtures.companyHECSession(loginData = companyLoginData, userAnswers = answers, crnBlocked = true)
          val ctutrAttempts = CtutrAttempts(validCRN, session.loginData.ggCredId, companyName, 1, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockCtutrAttemptsServiceGet(validCRN, session.loginData.ggCredId)(Right(Some(ctutrAttempts)))
            mockJourneyServiceUpdateAndNext(
              routes.CRNController.companyRegistrationNumber(),
              session,
              session.copy(crnBlocked = false)
            )(
              Right(mockNextCall)
            )
          }
          checkIsRedirect(performAction("crn" -> validCRN.value), mockNextCall)
        }

      }
    }

  }

}
