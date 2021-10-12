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
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.CompanyHECSession
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.CompanyLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.CompanyRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CRN, GGCredId}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.{CompanyHouseDetails, CompanyHouseName, Error}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{CompanyDetailsService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

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
  val validCRN                  =
    List(CRN("11123456"), CRN("1S1 23 45"), CRN("1S123456"), CRN("1s123456"), CRN("1S12345"), CRN("1112345"))
  val nonAlphaNumCRN            = List(CRN("$£%^&"), CRN("AA1244&"))
  val inValidCRN                =
    List(CRN("AAB3456"), CRN("12345AAA"))
  val companyHouseName          = CompanyHouseName("Test Tech Ltd")

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[CompanyDetailsService].toInstance(mockCompanyDetailsService)
  )

  val controller       = instanceOf[CRNController]
  val companyLoginData = CompanyLoginData(GGCredId(""), None, None)

  def mockFindCompany(crn: CRN)(
    result: Either[Error, Option[CompanyHouseDetails]]
  ) = (mockCompanyDetailsService
    .findCompany(_: CRN)(_: HeaderCarrier))
    .expects(crn, *)
    .returning(EitherT.fromEither(result))

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

          val answers = Fixtures.completeUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear
          )

          val updatedAnswers = IncompleteUserAnswers
            .fromCompleteAnswers(answers)
            .copy(
              taxSituation = None,
              crn = Some(validCRN(0))
            )
          val session        =
            CompanyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers, None, None, List.empty)
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
              input.attr("value") shouldBe validCRN(0).value

            }
          )

        }

      }

      "handling submit on the CRN page" must {

        def performAction(data: (String, String)*): Future[Result] =
          controller.companyRegistrationNumberSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

        behave like authAndSessionDataBehaviour(() => performAction())

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

        }

        "return an InternalServerError" when {

          val answers = Fixtures.completeUserAnswers(
            LicenceType.OperatorOfPrivateHireVehicles,
            LicenceTimeTrading.ZeroToTwoYears,
            LicenceValidityPeriod.UpToOneYear
          )
          val session =
            CompanyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers, None, None, List.empty)

          "there is an error updating and getting the next endpoint" in {
            val updatedAnswers = IncompleteUserAnswers
              .fromCompleteAnswers(answers)
              .copy(crn = Some(validCRN(0)))

            val updatedSession = session.copy(
              retrievedJourneyData = session.retrievedJourneyData.copy(companyName = Some(companyHouseName)),
              userAnswers = updatedAnswers
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockFindCompany(validCRN(0))(
                Right(Some(CompanyHouseDetails(companyHouseName)))
              )
              mockJourneyServiceUpdateAndNext(
                routes.CRNController.companyRegistrationNumber(),
                session,
                updatedSession
              )(
                Left(Error(""))
              )
            }

            status(performAction("crn" -> validCRN(0).value)) shouldBe INTERNAL_SERVER_ERROR

          }

        }

      }

      "redirect to the next page" when {

        def performAction(data: (String, String)*): Future[Result] =
          controller.companyRegistrationNumberSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

        def testNextCall(companyDetails: Option[CompanyHouseDetails]) =
          validCRN.foreach { crn =>
            withClue(s" For CRN : $crn") {

              val formattedCrn = CRN(crn.value.removeWhitespace.toUpperCase(Locale.UK))
              val answers      = CompleteUserAnswers(
                LicenceType.OperatorOfPrivateHireVehicles,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToOneYear,
                None,
                None,
                None,
                None,
                None,
                None,
                None
              )
              val session      =
                CompanyHECSession(companyLoginData, CompanyRetrievedJourneyData.empty, answers, None, None, List.empty)

              val updatedAnswers = IncompleteUserAnswers
                .fromCompleteAnswers(answers)
                .copy(
                  crn = Some(formattedCrn)
                )

              val updatedRetrievedJourneyData =
                session.retrievedJourneyData.copy(companyName = companyDetails.map(_.companyName))
              val updatedSession              =
                session.copy(retrievedJourneyData = updatedRetrievedJourneyData, userAnswers = updatedAnswers)

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
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

        "a valid CRN is submitted and company is found " in {
          testNextCall(Some(CompanyHouseDetails(companyHouseName)))
        }

        "a valid CRN is submitted but company is not found " in {
          testNextCall(None)
        }

      }

    }

  }

}
