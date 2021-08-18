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

import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.{CompleteUserAnswers, IncompleteUserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TaxSituationControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with JourneyServiceSupport {

  val mockTimeProvider = mock[TimeProvider]

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider)
  )

  def mockTimeProviderToday(d: LocalDate) = (mockTimeProvider.currentDate _).expects().returning(d)

  def taxYearMessage(startYear: Int, endYear: Int) =
    messageFromMessageKey("taxSituation.hint", startYear.toString, endYear.toString)

  val controller = instanceOf[TaxSituationController]

  val individuaRetrievedlData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyRetrievedData =
    CompanyRetrievedData(GGCredId(""), None, None)

  "TaxSituationController" when {

    "handling request to tax situation page " must {

      def performAction(): Future[Result] = controller.taxSituation(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "return an InternalServerError" when {

        "a licence type cannot be found in session" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "display the page" when {

        "the user has not previously answered the question " when {

          "licence Type is Driver of taxis and private hire vehicles" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(TimeUtils.today())
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )
          }

        }

        "the user has previously answered the question" in {
          val session =
            HECSession(
              individuaRetrievedlData,
              CompleteUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
                LicenceTimeTrading.TwoToFourYears,
                LicenceValidityPeriod.UpToThreeYears,
                TaxSituation.PAYE,
                EntityType.Individual
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
              mockPreviousCall
            )
            mockTimeProviderToday(TimeUtils.today())
          }

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxSituation.title"),
            { doc =>
              doc.select("#back").attr("href") shouldBe mockPreviousCall.url

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.attr("value") shouldBe "0"

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
            }
          )
        }

        "with tax year as 2019 to 2020" when {

          "today's date is start of the year" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2021, 1, 1))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                doc.select("#taxSituation-hint").text shouldBe taxYearMessage(2019, 2020)

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is more than six months from 6 april 2020" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2020, 10, 24))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                doc.select("#taxSituation-hint").text shouldBe taxYearMessage(2019, 2020)

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is exactly six months from 6 april 2020" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2020, 10, 6))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                doc.select("#taxSituation-hint").text shouldBe taxYearMessage(2019, 2020)

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

        }

        "with tax year as 2020 to 2021" when {

          "today's date is start of the year 2022" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2022, 1, 1))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                doc.select("#taxSituation-hint").text shouldBe taxYearMessage(2020, 2021)

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is more than six months from 6 april 2021" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2021, 10, 24))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                doc.select("#taxSituation-hint").text shouldBe taxYearMessage(2020, 2021)

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is exactly six months from 6 april 2021" in {
            val session = HECSession(
              individuaRetrievedlData,
              UserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2021, 10, 6))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title"),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                doc.select("#taxSituation-hint").text shouldBe taxYearMessage(2020, 2021)

                val options = doc.select(".govuk-radios__item")
                options.size() shouldBe 4

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

        }

      }

    }

    "handling submits on the tax situation page" must {
      def performAction(data: (String, String)*): Future[Result] =
        controller.taxSituationSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val answers        = UserAnswers.empty
        val updatedAnswers = UserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val session        = HECSession(individuaRetrievedlData, answers)
        val updatedSession = session.copy(userAnswers = updatedAnswers)

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("taxSituation.title"),
            messageFromMessageKey("taxSituation.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("taxSituation" -> Int.MaxValue.toString),
            messageFromMessageKey("taxSituation.title"),
            messageFromMessageKey("taxSituation.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation(), updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("taxSituation" -> "xyz"),
            messageFromMessageKey("taxSituation.title"),
            messageFromMessageKey("taxSituation.error.invalid")
          )
        }

      }

      "return an InternalServerError" when {

        "a licence type cannot be found in session" in {
          val session = HECSession(individuaRetrievedlData, UserAnswers.empty)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

        "the call to update and next fails" in {
          val answers        = UserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
          val updatedAnswers = UserAnswers.empty
            .copy(
              licenceType = Some(DriverOfTaxisAndPrivateHires),
              licenceValidityPeriod = None,
              taxSituation = Some(TaxSituation.PAYE)
            )
          val session        = HECSession(individuaRetrievedlData, answers)
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.TaxSituationController.taxSituation(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }

          status(performAction("taxSituation" -> "0")) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers        = UserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
            val updatedAnswers =
              answers.copy(taxSituation = Some(TaxSituation.PAYE))
            val session        = HECSession(individuaRetrievedlData, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.TaxSituationController.taxSituation(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("taxSituation" -> "0"), mockNextCall)

          }

          "the user has previously completed answering questions" in {
            val answers        = CompleteUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceExpiryDate(TimeUtils.today().minusDays(10L)),
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToThreeYears,
              TaxSituation.PAYE,
              EntityType.Individual
            )
            val updatedAnswers = IncompleteUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceExpiryDate(TimeUtils.today().minusDays(10L))),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToThreeYears),
              Some(TaxSituation.PAYE),
              Some(EntityType.Individual)
            )
            val session        = HECSession(individuaRetrievedlData, answers)
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.TaxSituationController.taxSituation(),
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("taxSituation" -> "0"), mockNextCall)
          }
        }

      }

    }
  }

}
