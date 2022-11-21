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

import org.jsoup.nodes.Element
import play.api.inject.bind
import play.api.mvc.Cookie
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.TaxCheckCodesDisplayed
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.IndividualLoginData
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.IndividualRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models._
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.LicenceType
import uk.gov.hmrc.hecapplicantfrontend.models.views.LicenceTypeOption
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService.InconsistentSessionState
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, AuditServiceSupport, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.utils.Fixtures

import java.time.{LocalDate, ZonedDateTime}
import scala.concurrent.ExecutionContext.Implicits.global

class TaxChecksListControllerSpec
    extends ControllerSpec
    with JourneyServiceSupport
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour
    with AuditServiceSupport {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService),
    bind[AuditService].toInstance(mockAuditService)
  )

  val controller = instanceOf[TaxChecksListController]

  val individualLoginData =
    IndividualLoginData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None, None)

  "TaxChecksListController" when {

    "handling requests to display the tax check codes page" must {

      def performAction(language: Language) =
        controller.unexpiredTaxChecks(FakeRequest().withCookies(Cookie("PLAY_LANG", language.code)))

      behave like (authAndSessionDataBehaviour(() => performAction(Language.English)))

      val answers = Fixtures.incompleteIndividualUserAnswers(
        Some(LicenceType.ScrapMetalMobileCollector)
      )

      "return an error when no tax checks found" in {
        val session =
          Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            answers,
            None,
            None,
            List.empty
          )
        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
        }

        assertThrows[InconsistentSessionState](await(performAction(Language.English)))

      }

      "display the page with tax checks grouped by licence type" in {
        val expiryDate                           = LocalDate.of(2020, 1, 10)
        val today                                = ZonedDateTime.now()
        val yesterday                            = today.minusDays(1)
        val dayBefore                            = today.minusDays(2)
        val scrapMetalCollectorTodayTaxCheck     = TaxCheckListItem(
          LicenceType.ScrapMetalMobileCollector,
          HECTaxCheckCode("2DYFK48KL"),
          expiryDate,
          today
        )
        val driverDayBeforeTaxCheck              = TaxCheckListItem(
          LicenceType.DriverOfTaxisAndPrivateHires,
          HECTaxCheckCode("XRCYRKA74"),
          expiryDate,
          dayBefore
        )
        val scrapMetalCollectorYesterdayTaxCheck = TaxCheckListItem(
          LicenceType.ScrapMetalMobileCollector,
          HECTaxCheckCode("THP3T2TXL"),
          expiryDate,
          yesterday
        )
        val scrapMetalDealerTodayTaxCheck        = TaxCheckListItem(
          LicenceType.ScrapMetalDealerSite,
          HECTaxCheckCode("GGP3T2TXL"),
          expiryDate,
          today
        )
        val operatorTodayTaxCheck                = TaxCheckListItem(
          LicenceType.OperatorOfPrivateHireVehicles,
          HECTaxCheckCode("FF3T2TXL"),
          expiryDate,
          today
        )

        val unsortedTaxChecks = List(
          driverDayBeforeTaxCheck,
          scrapMetalCollectorTodayTaxCheck,
          scrapMetalCollectorYesterdayTaxCheck,
          scrapMetalDealerTodayTaxCheck,
          operatorTodayTaxCheck
        )

        val session = Fixtures.individualHECSession(
          individualLoginData,
          IndividualRetrievedJourneyData.empty,
          answers,
          None,
          None,
          unsortedTaxChecks
        )

        inSequence {
          mockAuthWithNoRetrievals()
          mockGetSession(session)
          mockSendAuditEvent(
            TaxCheckCodesDisplayed(
              individualLoginData.ggCredId,
              unsortedTaxChecks.map(_.taxCheckCode),
              Language.English
            )
          )
        }

        def verifyLicenceGroup(
          element: Element,
          licenceType: LicenceType,
          expectedTaxChecks: List[TaxCheckListItem]
        ) = {
          val heading = element.select("h2")
          heading
            .text() shouldBe s"${messageFromMessageKey(s"licenceType.${LicenceTypeOption.licenceTypeOption(licenceType).messageKey}")}"
          val taxCheckItems = element.select(".existing-code")
          taxCheckItems.size() shouldBe expectedTaxChecks.size
          expectedTaxChecks.zipWithIndex.map { case (t, index) =>
            verifyTaxCheckListItem(taxCheckItems.get(index), t)
          }
        }

        def verifyTaxCheckListItem(element: Element, item: TaxCheckListItem) = {
          val keys = element.select(".govuk-summary-list__key")
          keys.size() shouldBe 2

          val expiryKey = keys.get(0)
          expiryKey.text() shouldBe s"${messageFromMessageKey("taxCheck.expiryKey")}"

          val taxCheckCodeKey = keys.get(1)
          taxCheckCodeKey
            .text() shouldBe s"${messageFromMessageKey("taxCheck.codeKey")}"

          val values = element.select(".govuk-summary-list__value")
          values.size() shouldBe 2

          val expiryValue = values.get(0)
          expiryValue.text() shouldBe TimeUtils.govDisplayFormat(item.expiresAfter)

          val taxCheckCodeValue = values.get(1)
          taxCheckCodeValue.text() shouldBe item.taxCheckCode.value.grouped(3).mkString(" ")

          val copyButton = element.select("button")
          copyButton.select(".copy-content").text()          shouldBe s"${messageFromMessageKey("button.copy")}"
          copyButton.select(".copied-content").text()        shouldBe s"${messageFromMessageKey("button.copied")}"
          copyButton.select(".govuk-visually-hidden").text() shouldBe s"${messageFromMessageKey(
              "taxChecksList.copyButtonScreenReaderText",
              messageFromMessageKey(s"licenceType.${LicenceTypeOption.licenceTypeOption(item.licenceType).messageKey}"),
              item.taxCheckCode.value
            )}"
          element.select("a.desktop-email-link").html()      shouldBe
            messageFromMessageKey(
              "taxCheck.emailLink",
              item.taxCheckCode.value
            )

          element.select("a.mobile-email-link").html() shouldBe
            messageFromMessageKey(
              "taxCheck.emailLink",
              item.taxCheckCode.value
            )
        }

        checkPageIsDisplayed(
          performAction(Language.English),
          messageFromMessageKey("taxChecksList.title"),
          doc => {
            val licenceGroups = doc.select(".licence-type-group")
            licenceGroups.size()                       shouldBe 4
            verifyLicenceGroup(
              licenceGroups.get(0),
              LicenceType.DriverOfTaxisAndPrivateHires,
              List(driverDayBeforeTaxCheck)
            )
            verifyLicenceGroup(
              licenceGroups.get(1),
              LicenceType.OperatorOfPrivateHireVehicles,
              List(operatorTodayTaxCheck)
            )
            verifyLicenceGroup(
              licenceGroups.get(2),
              LicenceType.ScrapMetalMobileCollector,
              List(scrapMetalCollectorTodayTaxCheck, scrapMetalCollectorYesterdayTaxCheck)
            )
            verifyLicenceGroup(
              licenceGroups.get(3),
              LicenceType.ScrapMetalDealerSite,
              List(scrapMetalDealerTodayTaxCheck)
            )
            doc.select("form").select("button").text() shouldBe messageFromMessageKey(
              "taxChecksList.button"
            )
          }
        )

      }

      "audit the correct language" in {
        List(
          Language.English,
          Language.Welsh
        ).foreach { lang =>
          val taxCheck = Fixtures.taxCheckListItem()
          val session  = Fixtures.individualHECSession(
            individualLoginData,
            IndividualRetrievedJourneyData.empty,
            answers,
            None,
            None,
            List(taxCheck)
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockSendAuditEvent(
              TaxCheckCodesDisplayed(individualLoginData.ggCredId, List(taxCheck.taxCheckCode), lang)
            )
          }

          status(performAction(lang)) shouldBe OK
        }

      }

    }

    "handling requests to submits on the tax check codes page" must {

      def performAction() = controller.unexpiredTaxChecksSubmit(FakeRequest().withMethod(POST))

      behave like (authAndSessionDataBehaviour(performAction))

      "return an error" when {

        "there is an error updating the session" in {
          inSequence {
            val session =
              Fixtures.individualHECSession(
                individualLoginData,
                emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck())
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.TaxChecksListController.unexpiredTaxChecks,
                session,
                session.copy(emailRequestedForTaxCheck = None)
              )(Left(Error("")))
            }

            assertThrows[RuntimeException](await(performAction()))
          }

        }

      }

      "redirect to the next page" when {

        "the session has been successfully updated" in {
          inSequence {
            val session =
              Fixtures.companyHECSession(
                Fixtures.companyLoginData(),
                emailRequestedForTaxCheck = Some(Fixtures.emailRequestedForTaxCheck())
              )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.TaxChecksListController.unexpiredTaxChecks,
                session,
                session.copy(emailRequestedForTaxCheck = None)
              )(Right(mockNextCall))
            }

            checkIsRedirect(performAction(), mockNextCall)
          }

        }

      }
    }

    "handling requests to send an email" must {

      def performAction(taxCheckCode: HECTaxCheckCode) =
        controller.sendEmail(taxCheckCode)(FakeRequest())

      behave like (authAndSessionDataBehaviour(() => performAction(HECTaxCheckCode(""))))

      "return an error" when {

        val taxCheckCode = HECTaxCheckCode("AAA222AAA")

        "the user has no unexpired tax checks in session" in {
          val session = Fixtures.individualHECSession(
            unexpiredTaxChecks = List.empty
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          assertThrows[InconsistentSessionState](await(performAction(taxCheckCode)))
        }

        "the user does not have a tax check in session with the " +
          "tax check code given in the URL" in {
            val session = Fixtures.individualHECSession(
              unexpiredTaxChecks = List(
                Fixtures.taxCheckListItem(
                  taxCheckCode = HECTaxCheckCode("BBB333BBB")
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
            }

            assertThrows[RuntimeException](await(performAction(taxCheckCode)))
          }

        "there is an error updating the session" in {
          val taxCheck = Fixtures.taxCheckListItem(taxCheckCode = taxCheckCode)
          val session  = Fixtures.individualHECSession(unexpiredTaxChecks = List(taxCheck))

          val updatedSession = session.copy(
            emailRequestedForTaxCheck = Some(
              EmailRequestedForTaxCheck(
                routes.TaxChecksListController.unexpiredTaxChecks.url,
                taxCheck
              )
            )
          )

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
            mockJourneyServiceUpdateAndNext(
              routes.TaxChecksListController.unexpiredTaxChecks,
              session,
              updatedSession
            )(Left(Error("")))
          }

          assertThrows[RuntimeException](await(performAction(taxCheckCode)))
        }

      }

      "redirect to the next page" when {

        "a tax check can be found with the given tax check code and the session is " +
          "successfully updated" in {
            val taxCheckCode = HECTaxCheckCode("AAA 2 22B BB ")
            val taxCheck     = Fixtures.taxCheckListItem(taxCheckCode = taxCheckCode)
            val session      = Fixtures.individualHECSession(unexpiredTaxChecks = List(taxCheck))

            val updatedSession = session.copy(
              emailRequestedForTaxCheck = Some(
                EmailRequestedForTaxCheck(
                  routes.TaxChecksListController.unexpiredTaxChecks.url,
                  taxCheck
                )
              )
            )

            inSequence {
              mockAuthWithNoRetrievals()
              mockGetSession(session)
              mockJourneyServiceUpdateAndNext(
                routes.TaxChecksListController.unexpiredTaxChecks,
                session,
                updatedSession
              )(Right(mockNextCall))
            }

            checkIsRedirect(
              performAction(HECTaxCheckCode("aaa222bbb")),
              mockNextCall
            )

          }

      }

    }

  }

}
