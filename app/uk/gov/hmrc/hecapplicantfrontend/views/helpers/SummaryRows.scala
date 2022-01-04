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

package uk.gov.hmrc.hecapplicantfrontend.views.helpers

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.hecapplicantfrontend.controllers.TaxSituationController.getTaxPeriodStrings
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.CompanyUserAnswers.CompleteCompanyUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.{CompleteUserAnswers, RetrievedJourneyData, TaxYear}
import uk.gov.hmrc.hecapplicantfrontend.models.views.{LicenceTimeTradingOption, LicenceTypeOption, LicenceValidityPeriodOption}
import uk.gov.hmrc.hecapplicantfrontend.models.IndividualUserAnswers.CompleteIndividualUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedJourneyData.CompanyRetrievedJourneyData
import uk.gov.hmrc.hecapplicantfrontend.models.hecTaxCheck.company.CTStatusResponse
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils
import uk.gov.hmrc.hecapplicantfrontend.models.views.EntityTypeOption
import uk.gov.hmrc.hecapplicantfrontend.models.views.YesNoOption
import uk.gov.hmrc.hecapplicantfrontend.models.views.TaxSituationOption

class SummaryRows {
  val messageKey = "checkYourAnswers"

  def summaryListRow(question: String, answer: String, changeLocation: Call, changeScreenReaderText: String)(implicit
    messages: Messages
  ): SummaryListRow =
    SummaryListRow(
      key = Key(content = Text(question), classes = "govuk-!-width-one-half"),
      value = Value(content = Text(answer)),
      actions = Some(
        Actions(
          items = Seq(
            ActionItem(
              href = changeLocation.url,
              content = Text(messages(s"$messageKey.change")),
              visuallyHiddenText = Some(changeScreenReaderText)
            )
          )
        )
      )
    )

  def licenceDetailsRows(completeAnswers: CompleteUserAnswers)(implicit messages: Messages): List[SummaryListRow] = {
    val licenceTypeRow: SummaryListRow =
      summaryListRow(
        messages("licenceType.title"),
        messages(
          s"licenceType.${LicenceTypeOption.licenceTypeOption(completeAnswers.foldOnEntityType(_.licenceType, _.licenceType)).messageKey}"
        ),
        routes.LicenceDetailsController.licenceType(),
        messages(s"$messageKey.licenceType.screenReaderText")
      )

    val licenceTimeTradingRow: SummaryListRow =
      summaryListRow(
        messages("licenceTimeTrading.title"),
        messages(
          s"licenceTimeTrading.${LicenceTimeTradingOption.licenceTimeTradingOption(completeAnswers.foldOnEntityType(_.licenceTimeTrading, _.licenceTimeTrading)).messageKey}"
        ),
        routes.LicenceDetailsController.licenceTimeTrading(),
        messages(s"$messageKey.licenceTimeTrading.screenReaderText")
      )

    val licenceValidityPeriodRow: SummaryListRow =
      summaryListRow(
        messages("licenceValidityPeriod.title"),
        messages(
          s"licenceValidityPeriod.${LicenceValidityPeriodOption.licenceValidityPeriodOption(completeAnswers.foldOnEntityType(_.licenceValidityPeriod, _.licenceValidityPeriod)).messageKey}"
        ),
        routes.LicenceDetailsController.recentLicenceLength(),
        messages(s"$messageKey.licenceValidityPeriod.screenReaderText")
      )

    List(
      licenceTypeRow,
      licenceTimeTradingRow,
      licenceValidityPeriodRow
    )
  }

  def companyRows(completeAnswers: CompleteCompanyUserAnswers, retrievedJourneyData: RetrievedJourneyData)(implicit
    messages: Messages
  ): List[SummaryListRow] = {

    val entityTypeRow             =
      summaryListRow(
        messages("entityType.title"),
        messages(s"entityType.${EntityTypeOption.entityTypeOption(completeAnswers.entityType).messageKey}"),
        routes.EntityTypeController.entityType(),
        messages(s"$messageKey.entityType.screenReaderText")
      )
    val crnRow                    =
      summaryListRow(
        messages("crn.title"),
        completeAnswers.crn.value,
        routes.CRNController.companyRegistrationNumber(),
        messages(s"$messageKey.crn.screenReaderText")
      )
    val ctutrRow                  =
      completeAnswers.ctutr.map { ctutr =>
        summaryListRow(
          messages("enterCtutr.title"),
          ctutr.value,
          routes.CompanyDetailsController.enterCtutr(),
          messages(s"$messageKey.ctutr.screenReaderText")
        )
      }
    val recentlyStartedTradingRow =
      completeAnswers.recentlyStartedTrading.map { recentlyStartedTrading =>
        summaryListRow(
          messages("recentlyStartedTrading.title"),
          messages(s"recentlyStartedTrading.${YesNoOption.yesNoOption(recentlyStartedTrading).messageKey}"),
          routes.CompanyDetailsController.recentlyStartedTrading(),
          messages(s"$messageKey.recentlyStartedTrading.screenReaderText")
        )
      }

    val chargeableForCTRow  =
      completeAnswers.chargeableForCT.map { chargeableForCT =>
        val latestAccountingPeriod = retrievedJourneyData match {
          case c: CompanyRetrievedJourneyData =>
            c.ctStatus match {
              case Some(CTStatusResponse(_, _, _, Some(latestAccountingPeriod))) => latestAccountingPeriod
              case _                                                             => sys.error("chargeableForCT answer found when no accounting period")
            }
          case _                              => sys.error("chargeableForCT answer found when company journey data not retrieved")
        }
        summaryListRow(
          messages("chargeableForCT.title", TimeUtils.govDisplayFormat(latestAccountingPeriod.endDate)),
          messages(s"chargeableForCT.${YesNoOption.yesNoOption(chargeableForCT).messageKey}"),
          routes.CompanyDetailsController.chargeableForCorporationTax(),
          messages(s"$messageKey.chargeableForCT.screenReaderText")
        )

      }
    val ctIncomeDeclaredRow =
      completeAnswers.ctIncomeDeclared.map { ctIncomeDeclared =>
        summaryListRow(
          messages("ctIncomeDeclared.title"),
          messages(s"ctIncomeDeclared.${YesNoOption.yesNoOption(ctIncomeDeclared).messageKey}"),
          routes.CompanyDetailsController.ctIncomeStatement(),
          messages(s"$messageKey.ctIncomeDeclared.screenReaderText")
        )
      }

    List(
      Some(entityTypeRow),
      Some(crnRow),
      ctutrRow,
      recentlyStartedTradingRow,
      chargeableForCTRow,
      ctIncomeDeclaredRow
    ).collect { case Some(r) => r }
  }

  def individualRows(
    completeAnswers: CompleteIndividualUserAnswers,
    relevantTaxYear: TaxYear
  )(implicit messages: Messages): List[SummaryListRow] = {
    val entityTypeRow =
      completeAnswers.entityType.map { entityType =>
        summaryListRow(
          messages("entityType.title"),
          messages(s"entityType.${EntityTypeOption.entityTypeOption(entityType).messageKey}"),
          routes.EntityTypeController.entityType(),
          messages(s"$messageKey.entityType.screenReaderText")
        )
      }
    val taxSituationRow = {
      val (startDate, endDate) = getTaxPeriodStrings(relevantTaxYear)
      summaryListRow(
        messages("taxSituation.title", startDate, endDate),
        messages(s"taxSituation.${TaxSituationOption.taxSituationOption(completeAnswers.taxSituation).messageKey}"),
        routes.TaxSituationController.taxSituation(),
        messages(s"$messageKey.taxSituation.screenReaderText")
      )
    }

    val saIncomeDeclaredRow =
      completeAnswers.saIncomeDeclared.map { saIncomeDeclared =>
        summaryListRow(
          messages("saIncomeDeclared.title"),
          messages(s"saIncomeDeclared.${YesNoOption.yesNoOption(saIncomeDeclared).messageKey}"),
          routes.SAController.saIncomeStatement(),
          messages(s"$messageKey.saIncomeDeclared.screenReaderText")
        )
      }

    List(
      entityTypeRow,
      Some(taxSituationRow),
      saIncomeDeclaredRow
    ).collect { case Some(r) => r }
  }

}
