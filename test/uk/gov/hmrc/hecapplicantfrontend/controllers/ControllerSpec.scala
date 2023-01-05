/*
 * Copyright 2023 HM Revenue & Customs
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

import com.google.inject.{Inject, Singleton}
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import play.api._
import uk.gov.hmrc.hecapplicantfrontend.utils.PlaySupport

import scala.concurrent.Future
import scala.collection.JavaConverters._

trait ControllerSpec extends PlaySupport {

  def checkIsRedirect(result: Future[Result], expectedRedirectLocation: String): Unit = {
    status(result) shouldBe SEE_OTHER

    redirectLocation(result) shouldBe Some(expectedRedirectLocation)
  }

  def checkIsRedirect(result: Future[Result], expectedRedirectLocation: Call): Unit =
    checkIsRedirect(result, expectedRedirectLocation.url)

  def messageFromMessageKey(messageKey: String, args: Any*)(implicit messagesApi: MessagesApi): String = {
    val m = messagesApi(messageKey, args: _*)
    if (m === messageKey) sys.error(s"Could not find message for key `$messageKey`")
    else m
  }

  def checkPageIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    contentChecks: Document => Unit = _ => (),
    expectedStatus: Int = OK
  ): Unit = {
    (status(result), redirectLocation(result)) shouldBe (expectedStatus -> None)

    val doc = Jsoup.parse(contentAsString(result))
    doc.select("h1").text shouldBe expectedTitle
    val bodyText = doc.select("body").text
    val regex    = """not_found_message\((.*?)\)""".r

    val regexResult = regex.findAllMatchIn(bodyText).toList
    if (regexResult.nonEmpty) fail(s"Missing message keys: ${regexResult.map(_.group(1)).mkString(", ")}")

    doc.select(".hmrc-language-select__list").text should include regex "English"
    doc.select(".hmrc-language-select__list").text should include regex "Cymraeg"

    contentChecks(doc)
  }

  def checkFormErrorIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    formError: String,
    expectedStatus: Int = OK,
    additionalChecks: Document => Unit = _ => ()
  ): Unit =
    checkPageIsDisplayed(
      result,
      expectedTitle,
      { doc =>
        val errorSummary = doc.select(".govuk-error-summary")
        errorSummary.select("a").text() shouldBe formError

        val inputErrorMessage = doc.select(".govuk-error-message")
        inputErrorMessage.text() shouldBe s"${messageFromMessageKey("generic.errorPrefix")}: $formError"
        additionalChecks(doc)
      },
      expectedStatus
    )

  def testRadioButtonOptions(
    doc: Document,
    expectedRadioLabels: List[String],
    expectedRadioHintTexts: List[Option[String]]
  ) = {
    val radios = doc.select(".govuk-radios__item").iterator().asScala.toList
    val labels = radios.map(_.select(".govuk-label").text())
    val hints  = radios.map(r => Option(r.select(".govuk-hint").text()).filter(_.nonEmpty))

    labels shouldBe expectedRadioLabels
    hints  shouldBe expectedRadioHintTexts
  }

}

@Singleton
class TestMessagesApiProvider @Inject() (
  environment: Environment,
  config: Configuration,
  langs: Langs,
  httpConfiguration: HttpConfiguration
) extends DefaultMessagesApiProvider(environment, config, langs, httpConfiguration) {

  val logger = Logger(this.getClass)

  override lazy val get: MessagesApi =
    new DefaultMessagesApi(
      loadAllMessages,
      langs,
      langCookieName,
      langCookieSecure,
      langCookieHttpOnly,
      langCookieSameSite,
      httpConfiguration,
      langCookieMaxAge
    ) {
      override protected def noMatch(key: String, args: Seq[Any])(implicit lang: Lang): String =
        sys.error(s"Could not find message for key: $key ${args.mkString("-")}")
    }

}
