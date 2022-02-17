package uk.gov.hmrc.hecapplicantfrontend.views.helpers

import play.api.data.Form
import play.api.i18n.Messages
import uk.gov.hmrc.govukfrontend.views.Aliases.Text
import uk.gov.hmrc.govukfrontend.views.viewmodels.errormessage.ErrorMessage

object FormErrorMessage {
  def formErrorMessage(form: Form[_], key: String)(implicit messages: Messages): Option[ErrorMessage] = form
    .error(key)
    .map(e =>
      ErrorMessage(
        content = Text(messages(s"${e.key}.${e.message}")),
        visuallyHiddenText = Some(messages("generic.errorPrefix"))
      )
    )
}
