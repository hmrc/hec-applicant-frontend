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

package uk.gov.hmrc.hecapplicantfrontend.views.helpers

import play.api.i18n.Messages
import play.api.mvc.Call
import uk.gov.hmrc.govukfrontend.views.html.components._
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.views.{LicenceTimeTradingOption, LicenceTypeOption, LicenceValidityPeriodOption}

class SummaryRows {
  val messageKey = "checkYourAnswers"

  def summaryListRow(question: String, answer: String, changeLocation: Call, changeScreenReaderText: String)(implicit
    messages: Messages
  ): SummaryListRow =
    SummaryListRow(
      key = Key(content = Text(question)),
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
}
