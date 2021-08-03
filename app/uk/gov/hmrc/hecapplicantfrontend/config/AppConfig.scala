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
import uk.gov.hmrc.play.bootstrap.config.ServicesConfig

import java.util.UUID

@Singleton
class AppConfig @Inject() (config: Configuration) extends ServicesConfig(config) {

  val contactFrontendUrl: String           = baseUrl("contact-frontend")
  val contactFormServiceIdentifier: String = getString("contact-frontend.serviceId")

  val welshLanguageSupportEnabled: Boolean = getConfBool("features.welsh-language-support", false)

  val betaFeedbackUrl: String = s"$contactFrontendUrl/contact/beta-feedback?service=$contactFormServiceIdentifier"

  val selfBaseUrl: String = getString("self.url")

  lazy val signInUrl: String = {
    val basGateway: String = getString("auth.bas-gateway.url")
    val origin: String     = getString("auth.gg.origin")
    s"$basGateway?continue=$selfBaseUrl${routes.StartController.start().url}&origin=$origin"
  }

  lazy val signOutUri: String = getString("auth.sign-out.uri")

  lazy val redirectToIvUplift: Result = {
    val ivUrl: String = getString("iv.url")

    val ivOrigin: String = getString("iv.origin")

    val (ivSuccessUrl: String, ivFailureUrl: String) = {
      val useRelativeUrls                          = getBoolean("iv.use-relative-urls")
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
