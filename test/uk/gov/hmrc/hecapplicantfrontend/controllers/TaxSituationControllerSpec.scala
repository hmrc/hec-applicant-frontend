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
import org.jsoup.nodes.Document
import play.api.inject.bind
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.{TimeProvider, TimeUtils}
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures
import uk.gov.hmrc.http.HeaderCarrier

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

  val mockTaxCheckService = mock[TaxCheckService]

  def mockGetSAStatus(utr: SAUTR, taxYear: TaxYear)(result: Either[Error, SAStatusResponse]) =
    (mockTaxCheckService
      .getSAStatus(_: SAUTR, _: TaxYear)(_: HeaderCarrier))
      .expects(utr, TaxYear(taxYear.startYear - 1), *)
      .returning(EitherT.fromEither(result))

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[TaxCheckService].toInstance(mockTaxCheckService)
  )

  def mockTimeProviderToday(d: LocalDate) = (mockTimeProvider.currentDate _).expects().returning(d)

  def taxYearMessage(startYear: Int, endYear: Int) =
    messageFromMessageKey("taxSituation.p1", startYear.toString, endYear.toString)

  val controller = instanceOf[TaxSituationController]

  val individualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)

  val companyRetrievedData =
    CompanyLoginData(GGCredId(""), None, None)

  val date = LocalDate.of(2020, 10, 24)

  "TaxSituationController" when {

    "handling request to tax situation page " must {

      def performAction(): Future[Result] = controller.taxSituation(FakeRequest())

      val allRadioTexts     = List(
        messageFromMessageKey("taxSituation.PA"),
        messageFromMessageKey("taxSituation.SA"),
        messageFromMessageKey("taxSituation.SAPAYE"),
        s"${messageFromMessageKey("taxSituation.NotChargeable")} ${messageFromMessageKey("taxSituation.NotChargeable.hint")}"
      )
      val nonPAYERadioTexts = List(
        messageFromMessageKey("taxSituation.SA"),
        s"${messageFromMessageKey("taxSituation.NotChargeable")} ${messageFromMessageKey("taxSituation.NotChargeable.hint")}"
      )

      def testAllTaxSituationsRadioOptions(doc: Document) =
        testRadioButtonOptions(doc, allRadioTexts)

      def testNonPAYETaxSituationsRadioOptions(doc: Document) =
        testRadioButtonOptions(doc, nonPAYERadioTexts)

      behave like authAndSessionDataBehaviour(performAction)

      "return a technical error" when {

        "a licence type cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[RuntimeException](await(performAction()))
        }

      }

      "display the page" when {

        "the user has not previously answered the question " when {

          "licence Type is Driver of taxis and private hire vehicles" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
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

        "the user has previously answered the question" ignore {
          val session =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.TwoToFourYears,
                LicenceValidityPeriod.UpToThreeYears,
                TaxSituation.PAYE,
                Some(YesNoAnswer.Yes)
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

          "today's date is start of the year" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
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

                testAllTaxSituationsRadioOptions(doc)

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is more than six months from 6 april 2020" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
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

                testAllTaxSituationsRadioOptions(doc)

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is exactly six months from 6 april 2020" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
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

                testAllTaxSituationsRadioOptions(doc)

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

          "today's date is start of the year 2022" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
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

                testAllTaxSituationsRadioOptions(doc)

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is more than six months from 6 april 2021" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
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

                testAllTaxSituationsRadioOptions(doc)

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true

                val form = doc.select("form")
                form
                  .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
              }
            )

          }

          "today's date is exactly six months from 6 april 2021" ignore {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
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

                testAllTaxSituationsRadioOptions(doc)

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

      "display only relevant options" when {

        "licence type = DriverOfTaxisAndPrivateHires" ignore {
          val session = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
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

              testAllTaxSituationsRadioOptions(doc)

              val selectedOptions = doc.select(".govuk-radios__input[checked]")
              selectedOptions.isEmpty shouldBe true

              val form = doc.select("form")
              form
                .attr("action") shouldBe routes.TaxSituationController.taxSituationSubmit().url
            }
          )
        }

        "licence type is not DriverOfTaxisAndPrivateHires" ignore {
          List(
            LicenceType.ScrapMetalDealerSite,
            LicenceType.ScrapMetalMobileCollector,
            LicenceType.OperatorOfPrivateHireVehicles
          ).foreach { licenceType =>
            withClue(s"For licence type $licenceType: ") {
              val session = Fixtures.individualHECSession(
                individualLoginData,
                IndividualRetrievedJourneyData.empty,
                IndividualUserAnswers.empty.copy(
                  licenceType = Some(licenceType)
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

                  testNonPAYETaxSituationsRadioOptions(doc)

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

    }

    "handling submits on the tax situation page" must {
      def performAction(data: (String, String)*): Future[Result] =
        controller.taxSituationSubmit(FakeRequest().withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val answers        = IndividualUserAnswers.empty
        val updatedAnswers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val session        =
          Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            answers
          )
        val updatedSession = session.copy(userAnswers = updatedAnswers)

        "nothing is submitted" ignore {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockTimeProviderToday(date)
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

        "an index is submitted which is too large" ignore {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockTimeProviderToday(date)
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

        "a value is submitted which is not a number" ignore {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockTimeProviderToday(date)
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

      "throw runtime exception" when {
        "retrieved user data in session is for a company" ignore {
          val session = CompanyHECSession.newSession(companyRetrievedData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[RuntimeException](await(performAction()))
        }
      }

      "return a technical error" when {

        "a licence type cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(date)
          }
          assertThrows[RuntimeException](await(performAction()))

        }

        "the call to update and next fails" in {
          val answers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
          val session = Fixtures.individualHECSession(
            individualLoginData.copy(sautr = Some(SAUTR("utr"))),
            IndividualRetrievedJourneyData.empty,
            answers
          )

          val updatedAnswers = IndividualUserAnswers.empty
            .copy(
              licenceType = Some(DriverOfTaxisAndPrivateHires),
              licenceValidityPeriod = None,
              taxSituation = Some(TaxSituation.PAYE)
            )
          val updatedSession = session.copy(userAnswers = updatedAnswers)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(date)
            mockJourneyServiceUpdateAndNext(
              routes.TaxSituationController.taxSituation(),
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("taxSituation" -> "0")))
        }

      }

      val optionIndexMap = Map(
        TaxSituation.PAYE          -> "0",
        TaxSituation.SA            -> "1",
        TaxSituation.SAPAYE        -> "2",
        TaxSituation.NotChargeable -> "3"
      )
      def testNonSA(taxSituation: TaxSituation) = {
        val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val individualData = individualLoginData.copy(sautr = Some(SAUTR("utr")))
        val session        = Fixtures.individualHECSession(individualData, IndividualRetrievedJourneyData.empty, answers)

        val updatedAnswers = answers.copy(taxSituation = Some(taxSituation))
        val updatedSession = session.copy(userAnswers = updatedAnswers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockTimeProviderToday(date)
          mockJourneyServiceUpdateAndNext(
            routes.TaxSituationController.taxSituation(),
            session,
            updatedSession
          )(
            Right(mockNextCall)
          )
        }

        checkIsRedirect(performAction("taxSituation" -> optionIndexMap(taxSituation)), mockNextCall)
      }

      "redirect to next page without fetching SA status" when {
        "tax situation = PAYE" in {
          testNonSA(TaxSituation.PAYE)
        }
        "tax situation = NotChargeable" in {
          testNonSA(TaxSituation.NotChargeable)
        }
      }

      def testSA(taxSituation: TaxSituation, statusResponse: SAStatusResponse) = {
        val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val individualData = individualLoginData.copy(sautr = Some(SAUTR("utr")))
        val session        = Fixtures.individualHECSession(individualData, IndividualRetrievedJourneyData.empty, answers)

        val updatedAnswers = answers.copy(taxSituation = Some(taxSituation))
        val updatedSession = session.copy(
          userAnswers = updatedAnswers,
          retrievedJourneyData = session.retrievedJourneyData.copy(saStatus = Some(statusResponse))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockTimeProviderToday(date)
          mockGetSAStatus(statusResponse.sautr, statusResponse.taxYear)(Right(statusResponse))
          mockJourneyServiceUpdateAndNext(
            routes.TaxSituationController.taxSituation(),
            session,
            updatedSession
          )(
            Right(mockNextCall)
          )
        }

        checkIsRedirect(performAction("taxSituation" -> optionIndexMap(taxSituation)), mockNextCall)
      }

      "redirect to next page after fetching SA status" when {
        "tax situation = SA" in {
          testSA(TaxSituation.SA, SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
        }
        "tax situation = SAPAYE" in {
          testSA(TaxSituation.SAPAYE, SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
        }
      }

      "throw technical error if call to fetch SA status fails" in {
        val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val individualData =
          individualLoginData.copy(sautr = Some(SAUTR("utr")))
        val session        =
          Fixtures.individualHECSession(individualData, IndividualRetrievedJourneyData.empty, answers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockTimeProviderToday(date)
          mockGetSAStatus(SAUTR("utr"), TaxYear(2020))(Left(Error("")))
        }
        assertThrows[RuntimeException](await(performAction("taxSituation" -> optionIndexMap(TaxSituation.SA))))

      }

      "redirect to the next page" when {

        "valid data is submitted and" when {

          "the user has not previously completed answering questions" in {
            val answers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
            val session = Fixtures.individualHECSession(
              individualLoginData.copy(sautr = Some(SAUTR("utr"))),
              IndividualRetrievedJourneyData.empty,
              answers
            )

            val updatedAnswers = answers.copy(taxSituation = Some(TaxSituation.PAYE))
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTimeProviderToday(date)
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
            val answers = Fixtures.completeIndividualUserAnswers(
              LicenceType.DriverOfTaxisAndPrivateHires,
              LicenceTimeTrading.ZeroToTwoYears,
              LicenceValidityPeriod.UpToThreeYears,
              TaxSituation.PAYE,
              Some(YesNoAnswer.Yes),
              Some(EntityType.Individual)
            )

            val session = Fixtures.individualHECSession(
              individualLoginData.copy(sautr = Some(SAUTR("utr"))),
              IndividualRetrievedJourneyData.empty,
              answers
            )

            val updatedAnswers = Fixtures.incompleteIndividualUserAnswers(
              Some(LicenceType.DriverOfTaxisAndPrivateHires),
              Some(LicenceTimeTrading.ZeroToTwoYears),
              Some(LicenceValidityPeriod.UpToThreeYears),
              Some(TaxSituation.PAYE),
              None,
              Some(EntityType.Individual)
            )
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTimeProviderToday(date)
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
