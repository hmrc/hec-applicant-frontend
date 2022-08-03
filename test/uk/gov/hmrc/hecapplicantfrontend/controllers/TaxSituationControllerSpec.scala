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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import cats.instances.future._
import play.api.inject.bind
import play.api.mvc.{Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.getTaxPeriodStrings
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.IncompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.individual.{SAStatus, SAStatusResponse}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType.DriverOfTaxisAndPrivateHires
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.models.views.LicenceTypeOption
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeProvider
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
      .expects(utr, taxYear, *)
      .returning(EitherT.fromEither(result))

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[TimeProvider].toInstance(mockTimeProvider),
    bind[TaxCheckService].toInstance(mockTaxCheckService)
  )

  def mockTimeProviderToday(d: LocalDate) = (mockTimeProvider.currentDate _).expects().returning(d)

  def mockStoreSession(individualSession: HECSession)(result: Either[Error, Unit]) = (mockSessionStore
    .store(_: HECSession)(_: Request[_]))
    .expects(individualSession, *)
    .returning(EitherT.fromEither(result))

  val controller = instanceOf[TaxSituationController]

  val individualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)

  val companyRetrievedData =
    CompanyLoginData(GGCredId(""), None, None, None)

  val date = LocalDate.of(2020, 10, 24)

  "TaxSituationController" when {

    "handling request to tax situation page " must {

      def performAction(): Future[Result] = controller.taxSituation(FakeRequest())

      val allRadioLabels    = List(
        "taxSituation.PA",
        "taxSituation.SA",
        "taxSituation.SAPAYE",
        "taxSituation.NotChargeable"
      ).map(messageFromMessageKey(_))
      val allRadioHintTexts = List(
        "taxSituation.PA.hint",
        "taxSituation.SA.hint",
        "taxSituation.SAPAYE.hint",
        "taxSituation.NotChargeable.hint"
      ).map(key => Some(messageFromMessageKey(key)))

      val nonPAYERadioLabels = List(
        "taxSituation.SA",
        "taxSituation.NotChargeable"
      ).map(messageFromMessageKey(_))

      val nonPAYERadioHintTexts = List(
        "taxSituation.SA.hint",
        "taxSituation.NotChargeable.hint"
      ).map(key => Some(messageFromMessageKey(key)))

      behave like authAndSessionDataBehaviour(performAction)

      "return a technical error" when {

        "a licence type cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))
        }

      }

      "display the page" when {

        val taxYear2020 = TaxYear(2020)

        def displayPageTest(
          value: Option[String],
          relevantIncomeTaxYearChanged: Boolean
        )(mockActions: => Unit) = {
          val (startDate, endDate) = getTaxPeriodStrings(taxYear2020)
          mockActions

          checkPageIsDisplayed(
            performAction(),
            messageFromMessageKey("taxSituation.title", startDate, endDate),
            { doc =>
              val expectedBack = if (relevantIncomeTaxYearChanged) "" else mockPreviousCall.url
              doc.select("#back").attr("href") shouldBe expectedBack

              val (expectedNotificationTitle, expectedNotificationContent) =
                if (relevantIncomeTaxYearChanged)
                  messageFromMessageKey("newTaxPeriod.notification.title") -> messageFromMessageKey(
                    "newTaxPeriod.notification.individual"
                  )
                else
                  ""                                                       -> ""

              doc.select(".govuk-notification-banner__title").text() shouldBe expectedNotificationTitle
              doc.select(".govuk-notification-banner__content").text() shouldBe expectedNotificationContent

              val options = doc.select(".govuk-radios__item")
              options.size() shouldBe 4

              val selectedOptions = doc.select(".govuk-radios__input[checked]")

              value match {
                case Some(index) => selectedOptions.attr("value") shouldBe index
                case None        => selectedOptions.isEmpty       shouldBe true
              }

              val form = doc.select("form")
              form
                .attr("action")            shouldBe routes.TaxSituationController.taxSituationSubmit.url
              form.select("legend").text() shouldBe messageFromMessageKey(
                s"taxSituation.label.${LicenceTypeOption.licenceTypeOption(LicenceType.DriverOfTaxisAndPrivateHires).messageKey}"
              )
            }
          )
        }

        "the user has not previously answered the question " when {

          "licence Type is Driver of taxis and private hire vehicles" in {
            val session = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(
                licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
              )
            )

            val updatedSession = session.copy(relevantIncomeTaxYear = taxYear2020.some)

            displayPageTest(None, false)(
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, session)(
                  mockPreviousCall
                )
                mockTimeProviderToday(LocalDate.of(2022, 1, 1))
                mockStoreSession(updatedSession)(Right(()))
              }
            )
          }

        }

        "the user has previously answered the question" when {

          "the tax year has not changed" in {
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
                ),
                relevantIncomeTaxYear = Some(taxYear2020)
              )

            displayPageTest(Some("0"), false)(
              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, session)(
                  mockPreviousCall
                )
              }
            )
          }
        }

        "the tax year has changed" in {
          val session        =
            Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              Fixtures.completeIndividualUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.TwoToFourYears,
                LicenceValidityPeriod.UpToThreeYears,
                TaxSituation.PAYE,
                Some(YesNoAnswer.Yes)
              ),
              relevantIncomeTaxYear = Some(taxYear2020),
              newRelevantIncomeTaxYear = Some(taxYear2020)
            )
          val updatedSession = session.copy(
            newRelevantIncomeTaxYear = None
          )

          displayPageTest(Some("0"), true)(
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockStoreSession(updatedSession)(Right(()))
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, session)(
                mockPreviousCall
              )
            }
          )
        }

      }

      def testPage(currentDate: LocalDate, taxYear: Int) = {
        val session              = Fixtures.individualHECSession(
          individualLoginData,
          IndividualRetrievedJourneyData.empty,
          IndividualUserAnswers.empty.copy(
            licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires)
          )
        )
        val updatedSession       = session.copy(relevantIncomeTaxYear = TaxYear(taxYear).some)
        val (startDate, endDate) = getTaxPeriodStrings(TaxYear(taxYear))
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, session)(
            mockPreviousCall
          )
          mockTimeProviderToday(currentDate)
          mockStoreSession(updatedSession)(Right(()))
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("taxSituation.title", startDate, endDate),
          { doc =>
            doc.select("#back").attr("href") shouldBe mockPreviousCall.url

            testRadioButtonOptions(doc, allRadioLabels, allRadioHintTexts)

            val selectedOptions = doc.select(".govuk-radios__input[checked]")
            selectedOptions.isEmpty shouldBe true
            val form = doc.select("form")
            form
              .attr("action")            shouldBe routes.TaxSituationController.taxSituationSubmit.url
            form.select("legend").text() shouldBe messageFromMessageKey(
              s"taxSituation.label.${LicenceTypeOption.licenceTypeOption(LicenceType.DriverOfTaxisAndPrivateHires).messageKey}"
            )
          }
        )

      }

      "with tax year as 2019 to 2020" when {

        "today's date is start of the year" in {
          testPage(LocalDate.of(2020, 10, 24), 2019)
        }

        "today's date is more than six months from 6 april 2020" in {
          testPage(LocalDate.of(2021, 1, 1), 2019)
        }

        "today's date is exactly six months from 6 april 2020" in {
          testPage(LocalDate.of(2020, 10, 6), 2019)
        }

      }

      "with tax year as 2020 to 2021" when {

        "today's date is start of the year 2022" in {
          testPage(LocalDate.of(2022, 1, 1), 2020)
        }

        "today's date is more than six months from 6 april 2021" in {
          testPage(LocalDate.of(2021, 10, 24), 2020)
        }

        "today's date is exactly six months from 6 april 2021" in {
          testPage(LocalDate.of(2021, 10, 6), 2020)
        }

        "display only relevant options" when {

          "licence type = DriverOfTaxisAndPrivateHires" in {

            val session              = Fixtures.individualHECSession(
              individualLoginData,
              IndividualRetrievedJourneyData.empty,
              IndividualUserAnswers.empty.copy(licenceType = Some(LicenceType.DriverOfTaxisAndPrivateHires))
            )
            val updatedSession       = session.copy(relevantIncomeTaxYear = TaxYear(2020).some)
            val (startDate, endDate) = getTaxPeriodStrings(TaxYear(2020))
            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, session)(
                mockPreviousCall
              )
              mockTimeProviderToday(LocalDate.of(2022, 1, 1))
              mockStoreSession(updatedSession)(Right(()))
            }

            checkPageIsDisplayed(
              performAction(),
              messageFromMessageKey("taxSituation.title", startDate, endDate),
              { doc =>
                doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                testRadioButtonOptions(doc, allRadioLabels, allRadioHintTexts)

                val selectedOptions = doc.select(".govuk-radios__input[checked]")
                selectedOptions.isEmpty shouldBe true
                val form = doc.select("form")
                form
                  .attr("action")            shouldBe routes.TaxSituationController.taxSituationSubmit.url
                form.select("legend").text() shouldBe messageFromMessageKey(
                  s"taxSituation.label.${LicenceTypeOption.licenceTypeOption(LicenceType.DriverOfTaxisAndPrivateHires).messageKey}"
                )
              }
            )
          }

          "licence type is not DriverOfTaxisAndPrivateHires" in {
            List(
              LicenceType.ScrapMetalDealerSite,
              LicenceType.ScrapMetalMobileCollector,
              LicenceType.OperatorOfPrivateHireVehicles,
              LicenceType.BookingOffice
            ).foreach { licenceType =>
              withClue(s"For licence type $licenceType: ") {
                val session              = Fixtures.individualHECSession(
                  individualLoginData,
                  IndividualRetrievedJourneyData.empty,
                  IndividualUserAnswers.empty.copy(
                    licenceType = Some(licenceType)
                  )
                )
                val updatedSession       = session.copy(relevantIncomeTaxYear = TaxYear(2020).some)
                val (startDate, endDate) = getTaxPeriodStrings(TaxYear(2020))
                inSequence {
                  mockAuthWithNoRetrievals()
                  mockGetSession(session)
                  mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, session)(
                    mockPreviousCall
                  )
                  mockTimeProviderToday(LocalDate.of(2022, 1, 1))
                  mockStoreSession(updatedSession)(Right(()))
                }

                checkPageIsDisplayed(
                  performAction(),
                  messageFromMessageKey("taxSituation.title", startDate, endDate),
                  { doc =>
                    doc.select("#back").attr("href") shouldBe mockPreviousCall.url

                    testRadioButtonOptions(doc, nonPAYERadioLabels, nonPAYERadioHintTexts)

                    val selectedOptions = doc.select(".govuk-radios__input[checked]")
                    selectedOptions.isEmpty shouldBe true

                    val form = doc.select("form")
                    form
                      .attr("action")            shouldBe routes.TaxSituationController.taxSituationSubmit.url
                    form.select("legend").text() shouldBe messageFromMessageKey(
                      s"taxSituation.label.${LicenceTypeOption.licenceTypeOption(licenceType).messageKey}"
                    )
                  }
                )
              }
            }
          }
        }

      }

    }

    "handling submits on the tax situation page" must {
      def performAction(data: (String, String)*): Future[Result] =
        controller.taxSituationSubmit(FakeRequest().withMethod(POST).withFormUrlEncodedBody(data: _*))

      behave like authAndSessionDataBehaviour(() => performAction())

      "show a form error" when {

        val answers              = IndividualUserAnswers.empty
        val updatedAnswers       = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val session              =
          Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            answers,
            relevantIncomeTaxYear = TaxYear(2020).some
          )
        val updatedSession       = session.copy(userAnswers = updatedAnswers)
        val (startDate, endDate) = getTaxPeriodStrings(TaxYear(2020))

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("taxSituation.title", startDate, endDate),
            messageFromMessageKey("taxSituation.error.required")
          )
        }

        "an index is submitted which is too large" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("taxSituation" -> Int.MaxValue.toString),
            messageFromMessageKey("taxSituation.title", startDate, endDate),
            messageFromMessageKey("taxSituation.error.invalid")
          )
        }

        "a value is submitted which is not a number" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(updatedSession)
            mockJourneyServiceGetPrevious(routes.TaxSituationController.taxSituation, updatedSession)(
              mockPreviousCall
            )
          }

          checkFormErrorIsDisplayed(
            performAction("taxSituation" -> "xyz"),
            messageFromMessageKey("taxSituation.title", startDate, endDate),
            messageFromMessageKey("taxSituation.error.invalid")
          )
        }

      }

      "throw runtime exception" when {
        "retrieved user data in session is for a company" in {
          val session = CompanyHECSession.newSession(companyRetrievedData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction()))
        }

      }

      "return a technical error" when {

        "a licence type cannot be found in session" in {
          val session = IndividualHECSession.newSession(individualLoginData)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }
          assertThrows[InconsistentSessionState](await(performAction()))

        }

        "the call to update and next fails" in {
          val answers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
          val session = Fixtures.individualHECSession(
            individualLoginData.copy(sautr = Some(SAUTR("utr"))),
            IndividualRetrievedJourneyData.empty,
            answers,
            relevantIncomeTaxYear = Some(TaxYear(2020))
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
            mockJourneyServiceUpdateAndNext(
              routes.TaxSituationController.taxSituation,
              session,
              updatedSession
            )(
              Left(Error(new Exception))
            )
          }
          assertThrows[RuntimeException](await(performAction("taxSituation" -> "0")))
        }

        "relevant tax year is not in session " in {
          val answers = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
          val session = Fixtures.individualHECSession(
            individualLoginData.copy(sautr = Some(SAUTR("utr"))),
            IndividualRetrievedJourneyData.empty,
            answers
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction("taxSituation" -> "0")))
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
        val session        = Fixtures.individualHECSession(
          individualData,
          IndividualRetrievedJourneyData.empty,
          answers,
          relevantIncomeTaxYear = TaxYear(2020).some
        )

        val updatedAnswers = answers.copy(taxSituation = Some(taxSituation))
        val updatedSession = session.copy(userAnswers = updatedAnswers)

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockJourneyServiceUpdateAndNext(
            routes.TaxSituationController.taxSituation,
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
        val session        = Fixtures.individualHECSession(
          individualData,
          IndividualRetrievedJourneyData.empty,
          answers,
          relevantIncomeTaxYear = TaxYear(2020).some
        )

        val updatedAnswers = answers.copy(taxSituation = Some(taxSituation))
        val updatedSession = session.copy(
          userAnswers = updatedAnswers,
          retrievedJourneyData = session.retrievedJourneyData.copy(saStatus = Some(statusResponse))
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockGetSAStatus(statusResponse.sautr, TaxYear(2020))(Right(statusResponse))
          mockJourneyServiceUpdateAndNext(
            routes.TaxSituationController.taxSituation,
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
          testSA(TaxSituation.SA, individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
        }

        "tax situation = SAPAYE" in {
          testSA(TaxSituation.SAPAYE, individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound))
        }

      }

      "throw technical error if call to fetch SA status fails" in {
        val answers        = IndividualUserAnswers.empty.copy(licenceType = Some(DriverOfTaxisAndPrivateHires))
        val individualData =
          individualLoginData.copy(sautr = Some(SAUTR("utr")))
        val session        =
          Fixtures.individualHECSession(
            individualData,
            IndividualRetrievedJourneyData.empty,
            answers,
            relevantIncomeTaxYear = TaxYear(2020).some
          )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
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
              answers,
              relevantIncomeTaxYear = TaxYear(2020).some
            )

            val updatedAnswers = answers.copy(taxSituation = Some(TaxSituation.PAYE))
            val updatedSession = session.copy(userAnswers = updatedAnswers)

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.TaxSituationController.taxSituation,
                session,
                updatedSession
              )(
                Right(mockNextCall)
              )
            }

            checkIsRedirect(performAction("taxSituation" -> "0"), mockNextCall)
          }

          "the user has previously completed answering questions and" when {

            "the answer has not changed" in {
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
                answers,
                relevantIncomeTaxYear = TaxYear(2020).some
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockJourneyServiceUpdateAndNext(
                  routes.TaxSituationController.taxSituation,
                  session,
                  session
                )(
                  Right(mockNextCall)
                )
              }

              checkIsRedirect(performAction("taxSituation" -> "0"), mockNextCall)
            }

            "the answer has changed" in {
              val answers = Fixtures.completeIndividualUserAnswers(
                LicenceType.DriverOfTaxisAndPrivateHires,
                LicenceTimeTrading.ZeroToTwoYears,
                LicenceValidityPeriod.UpToThreeYears,
                TaxSituation.SA,
                Some(YesNoAnswer.Yes),
                Some(EntityType.Individual)
              )

              val session = Fixtures.individualHECSession(
                individualLoginData.copy(sautr = Some(SAUTR("utr"))),
                IndividualRetrievedJourneyData.empty,
                answers,
                relevantIncomeTaxYear = TaxYear(2020).some
              )

              val saStatusResponse = individual.SAStatusResponse(SAUTR("utr"), TaxYear(2020), SAStatus.ReturnFound)

              val updatedAnswers = Fixtures.incompleteIndividualUserAnswers(
                Some(LicenceType.DriverOfTaxisAndPrivateHires),
                Some(LicenceTimeTrading.ZeroToTwoYears),
                Some(LicenceValidityPeriod.UpToThreeYears),
                Some(TaxSituation.SAPAYE),
                None,
                Some(EntityType.Individual)
              )
              val updatedSession = session.copy(
                userAnswers = updatedAnswers,
                retrievedJourneyData = IndividualRetrievedJourneyData(Some(saStatusResponse))
              )

              inSequence {
                mockAuthWithNoRetrievals()
                mockGetSession(session)
                mockGetSAStatus(saStatusResponse.sautr, TaxYear(2020))(Right(saStatusResponse))
                mockJourneyServiceUpdateAndNext(
                  routes.TaxSituationController.taxSituation,
                  session,
                  updatedSession
                )(
                  Right(mockNextCall)
                )
              }

              checkIsRedirect(performAction("taxSituation" -> "2"), mockNextCall)
            }

          }
        }

      }

    }

    "handling requests to the determineIfRelevantIncomeTaxYearChanged endpoint" must {

      def performAction(): Future[Result] =
        controller.determineIfRelevantIncomeTaxYearChanged(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      val today = LocalDate.now()

      val calculatedRelevantIncomeTaxYear = TaxSituationController.getRelevantIncomeTaxYear(today)
      val differentRelevantIncomeTaxYear  = TaxYear(calculatedRelevantIncomeTaxYear.startYear - 1)

      "return an error" when {

        "the session is for a company" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.companyHECSession())
          }

          an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
        }

        "there is no relevant income tax year in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession(relevantIncomeTaxYear = None))
          }

          an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
        }

        "there is an error storing a new relevant income tax year in session" in {
          val session = Fixtures.individualHECSession(relevantIncomeTaxYear = Some(differentRelevantIncomeTaxYear))

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockTimeProviderToday(today)
            mockStoreSession(session.copy(newRelevantIncomeTaxYear = Some(calculatedRelevantIncomeTaxYear)))(
              Left(Error(""))
            )
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction()))
        }

      }

      "redirect to the tax situation page" when {

        "the newly calculated relevant income tax year is the same as in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession(relevantIncomeTaxYear = Some(calculatedRelevantIncomeTaxYear)))
            mockTimeProviderToday(today)
          }

          checkIsRedirect(performAction(), routes.TaxSituationController.taxSituation)
        }

      }

      "redirect to the proceedWithNewRelevantIncomeTaxYear page" when {

        "a relevant income tax year has been calculated which is different than the one stored in session " +
          "and the new tax year is stored successfully" in {
            val session = Fixtures.individualHECSession(relevantIncomeTaxYear = Some(differentRelevantIncomeTaxYear))

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockTimeProviderToday(today)
              mockStoreSession(session.copy(newRelevantIncomeTaxYear = Some(calculatedRelevantIncomeTaxYear)))(
                Right(())
              )
            }

            checkIsRedirect(performAction(), routes.TaxSituationController.proceedWithNewRelevantIncomeTaxYear)
          }

      }

    }

    "handling requests to the proceedWithNewRelevantIncomeTaxYear endpoint" must {

      def performAction(): Future[Result] =
        controller.proceedWithNewRelevantIncomeTaxYear(FakeRequest())

      behave like authAndSessionDataBehaviour(performAction)

      "display the page" in {
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(Fixtures.individualHECSession())
        }

        checkPageIsDisplayed(
          performAction(),
          messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.title"),
          { doc =>
            doc.select("#back").attr("href") shouldBe routes.CheckYourAnswersController.checkYourAnswers.url

            testRadioButtonOptions(
              doc,
              List(
                messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.yes"),
                messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.no")
              ),
              List(None, None)
            )

            doc
              .select("form")
              .attr("action") shouldBe routes.TaxSituationController.proceedWithNewRelevantIncomeTaxYearSubmit.url

          }
        )
      }
    }

    "handling submits to the proceedWithNewRelevantIncomeTaxYear endpoint" must {

      def performAction(formData: (String, String)*): Future[Result] =
        controller.proceedWithNewRelevantIncomeTaxYearSubmit(
          FakeRequest().withMethod(POST).withFormUrlEncodedBody(formData: _*)
        )

      val newRelevantIncomeTaxYear = TaxYear(2022)

      behave like authAndSessionDataBehaviour(() => performAction())

      "return a technical error" when {

        "the session is for a company" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.companyHECSession())
          }

          an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
        }

        "a new relevant income tax year cannot be found in session" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession(newRelevantIncomeTaxYear = None))
          }

          an[InconsistentSessionState] shouldBe thrownBy(await(performAction()))
        }

        "there is an error storing the new tax year" in {
          val userAnswers    = Fixtures.completeIndividualUserAnswers()
          val session        = Fixtures.individualHECSession(
            newRelevantIncomeTaxYear = Some(newRelevantIncomeTaxYear),
            userAnswers = userAnswers
          )
          val updatedSession =
            session.copy(
              relevantIncomeTaxYear = Some(newRelevantIncomeTaxYear),
              userAnswers = IncompleteIndividualUserAnswers(
                userAnswers.licenceType.some,
                userAnswers.licenceTimeTrading.some,
                userAnswers.licenceValidityPeriod.some,
                None,
                None,
                userAnswers.entityType
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Left(Error("")))
          }

          a[RuntimeException] shouldBe thrownBy(await(performAction("proceedWithNewRelevantIncomeTaxYear" -> "0")))
        }

      }

      "return a form error" when {

        "nothing is submitted" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession(newRelevantIncomeTaxYear = Some(newRelevantIncomeTaxYear)))
          }

          checkFormErrorIsDisplayed(
            performAction(),
            messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.title"),
            messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.error.required")
          )
        }

        "an index is submitted which is not recognised" in {
          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(Fixtures.individualHECSession(newRelevantIncomeTaxYear = Some(newRelevantIncomeTaxYear)))
          }

          checkFormErrorIsDisplayed(
            performAction("proceedWithNewRelevantIncomeTaxYear" -> "abc"),
            messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.title"),
            messageFromMessageKey("proceedWithNewRelevantIncomeTaxYear.error.invalid")
          )
        }

      }

      "update the session and redirect to the next page" when {

        "the user submits yes" in {
          val userAnswers    = Fixtures.completeIndividualUserAnswers()
          val session        = Fixtures.individualHECSession(
            newRelevantIncomeTaxYear = Some(newRelevantIncomeTaxYear),
            userAnswers = userAnswers
          )
          val updatedSession =
            session.copy(
              relevantIncomeTaxYear = Some(newRelevantIncomeTaxYear),
              userAnswers = IncompleteIndividualUserAnswers(
                userAnswers.licenceType.some,
                userAnswers.licenceTimeTrading.some,
                userAnswers.licenceValidityPeriod.some,
                None,
                None,
                userAnswers.entityType
              )
            )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("proceedWithNewRelevantIncomeTaxYear" -> "0"),
            routes.TaxSituationController.taxSituation
          )
        }

        "the user submits no" in {
          val userAnswers    = Fixtures.completeIndividualUserAnswers()
          val session        = Fixtures.individualHECSession(
            newRelevantIncomeTaxYear = Some(newRelevantIncomeTaxYear),
            userAnswers = userAnswers
          )
          val updatedSession = session.copy(newRelevantIncomeTaxYear = None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockStoreSession(updatedSession)(Right(()))
          }

          checkIsRedirect(
            performAction("proceedWithNewRelevantIncomeTaxYear" -> "1"),
            routes.CheckYourAnswersController.checkYourAnswers
          )
        }

      }

    }

  }

}
