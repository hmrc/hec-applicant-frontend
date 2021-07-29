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

package uk.gov.hmrc.hecapplicantfrontend.config

import cats.instances.char._
import cats.syntax.eq._
import javax.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.mvc.Result
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.auth.core.ConfidenceLevel
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes

import java.util.UUID

@Singleton
class AppConfig @Inject() (config: Configuration) {

  val welshLanguageSupportEnabled: Boolean =
    config.getOptional[Boolean]("features.welsh-language-support").getOrElse(false)

  val selfBaseUrl: String = config.get[String]("self.url")

  lazy val signInUrl: String = {
    val basGateway: String = config.get[String]("auth.bas-gateway.url")
    val origin: String     = config.get[String]("auth.gg.origin")
    s"$basGateway?continue=$selfBaseUrl${routes.StartController.start().url}&origin=$origin"
  }

  lazy val signOutUri: String = config.get[String]("auth.sign-out.uri")

  lazy val redirectToIvUplift: Result = {
    val ivUrl: String = config.get[String]("iv.url")

    val ivOrigin: String = config.get[String]("iv.origin")

    val (ivSuccessUrl: String, ivFailureUrl: String) = {
      val useRelativeUrls                          = config.get[Boolean]("iv.use-relative-urls")
      val (successRelativeUrl, failureRelativeUrl) =
        routes.StartController.start().url ->
          routes.IvFailureController.ivFailure(UUID.randomUUID()).url.takeWhile(_ =!= '?')

      if (useRelativeUrls)
        successRelativeUrl                 -> failureRelativeUrl
      else
        s"$selfBaseUrl$successRelativeUrl" -> s"$selfBaseUrl$failureRelativeUrl"
    }

    val redirectToIvUrl: String = s"$ivUrl/mdtp/uplift"

    Redirect(
      redirectToIvUrl,
      Map(
        "origin"          -> Seq(ivOrigin),
        "confidenceLevel" -> Seq(ConfidenceLevel.L250.level.toString),
        "completionURL"   -> Seq(ivSuccessUrl),
        "failureURL"      -> Seq(ivFailureUrl)
      )
    )
  }

}
