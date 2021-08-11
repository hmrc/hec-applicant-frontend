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

import com.google.inject.{Inject, Singleton}
import com.typesafe.config.ConfigFactory
import org.jsoup.Jsoup
import org.jsoup.nodes.Document
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.http.HttpConfiguration
import play.api.i18n._
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.mvc.{Call, Result}
import play.api.test.Helpers._
import play.api._
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig

import scala.concurrent.Future
import scala.reflect.ClassTag

trait ControllerSpec extends AnyWordSpec with Matchers with BeforeAndAfterAll with MockFactory {

  implicit val lang: Lang = Lang("en")

  def overrideBindings: List[GuiceableModule] = List.empty[GuiceableModule]

  lazy val additionalConfig = Configuration()

  def buildFakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(
        Configuration(
          ConfigFactory.parseString(
            """
            | microservice.metrics.graphite.enabled = false 
      | """.stripMargin
          )
        ).withFallback(additionalConfig)
      )
      .disable[uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore]
      .overrides(overrideBindings: _*)
      .overrides(bind[MessagesApi].toProvider[TestMessagesApiProvider])
      .build()

  lazy val fakeApplication: Application = buildFakeApplication()

  lazy val appConfig: AppConfig = instanceOf[AppConfig]

  abstract override def beforeAll(): Unit = {
    Play.start(fakeApplication)
    super.beforeAll()
  }

  override def afterAll(): Unit = {
    Play.stop(fakeApplication)
    super.afterAll()
  }
  implicit lazy val messagesApi = instanceOf[MessagesApi]

  implicit lazy val messages = MessagesImpl(lang, messagesApi)

  def instanceOf[A : ClassTag]: A = fakeApplication.injector.instanceOf[A]

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

    contentChecks(doc)
  }

  def checkFormErrorIsDisplayed(
    result: Future[Result],
    expectedTitle: String,
    formError: String,
    expectedStatus: Int = OK
  ): Unit =
    checkPageIsDisplayed(
      result,
      expectedTitle,
      { doc =>
        val errorSummary = doc.select(".govuk-error-summary")
        errorSummary.select("a").text() shouldBe formError

        val inputErrorMessage = doc.select(".govuk-error-message")
        inputErrorMessage.text() shouldBe s"Error: $formError"
      },
      expectedStatus
    )

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
      override protected def noMatch(key: String, args: Seq[Any])(implicit lang: Lang): String = {
        logger.error(s"Could not find message for key: $key ${args.mkString("-")}")
        s"""not_found_message("$key")"""
      }
    }

}
