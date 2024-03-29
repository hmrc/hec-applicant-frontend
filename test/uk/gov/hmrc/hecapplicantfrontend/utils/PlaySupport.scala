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

package uk.gov.hmrc.hecapplicantfrontend.utils

import com.typesafe.config.ConfigFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.i18n.{Lang, MessagesApi, MessagesImpl}
import play.api.inject.bind
import play.api.inject.guice.{GuiceApplicationBuilder, GuiceableModule}
import play.api.{Application, Configuration, Play}
import uk.gov.hmrc.hecapplicantfrontend.config.AppConfig
import uk.gov.hmrc.hecapplicantfrontend.controllers.TestMessagesApiProvider

import scala.reflect.ClassTag

trait PlaySupport extends AnyWordSpec with Matchers with BeforeAndAfterAll with MockFactory {

  implicit val lang: Lang = Lang("en")

  def overrideBindings: List[GuiceableModule] = List.empty[GuiceableModule]

  def additionalConfig = Configuration()

  def buildFakeApplication(): Application =
    new GuiceApplicationBuilder()
      .configure(
        Configuration(
          ConfigFactory.parseString(
            """
              | microservice.metrics.graphite.enabled = false 
              | metrics.enabled = false
              | metrics.jvm = false
              | 
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

  override def afterAll(): Unit              = {
    Play.stop(fakeApplication)
    super.afterAll()
  }
  implicit lazy val messagesApi: MessagesApi = instanceOf[MessagesApi]

  implicit lazy val messages: MessagesImpl = MessagesImpl(lang, messagesApi)

  def instanceOf[A : ClassTag]: A = fakeApplication.injector.instanceOf[A]

}
