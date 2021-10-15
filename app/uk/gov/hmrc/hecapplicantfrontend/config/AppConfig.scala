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
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig

import java.util.UUID
import scala.concurrent.duration.FiniteDuration

@Singleton
class AppConfig @Inject() (config: Configuration, contactFrontendConfig: ContactFrontendConfig) {

  val platformHost: Option[String] = config.getOptional[String]("platform.frontend.host")
  val contactFrontendUrl: String   =
    contactFrontendConfig.baseUrl.getOrElse(sys.error("Could not find config for contact frontend url"))

  val contactFormServiceIdentifier: String =
    contactFrontendConfig.serviceId.getOrElse(sys.error("Could not find config for contact frontend service id"))

  val betaFeedbackUrl: String = s"$contactFrontendUrl/contact/beta-feedback?service=$contactFormServiceIdentifier"

  val welshLanguageSupportEnabled: Boolean =
    config.getOptional[Boolean]("features.welsh-language-support").getOrElse(false)

  val selfBaseUrl: String = platformHost.getOrElse(config.get[String]("self.url"))

  lazy val signInUrl: String = {
    val basGateway: String = config.get[String]("auth.bas-gateway.url")
    val origin: String     = config.get[String]("auth.gg.origin")
    s"$basGateway?continue=$selfBaseUrl${routes.StartController.start().url}&origin=$origin"
  }

  lazy val signOutUri: String = config.get[String]("auth.sign-out.uri")

  val authTimeoutSeconds: Int          = config.get[FiniteDuration]("auth.sign-out.inactivity-timeout").toSeconds.toInt
  val authTimeoutCountdownSeconds: Int =
    config.get[FiniteDuration]("auth.sign-out.inactivity-countdown").toSeconds.toInt

  lazy val redirectToIvUplift: Result = {
    val ivUrl: String = platformHost.getOrElse(config.get[String]("iv.url"))

    val ivOrigin: String = config.get[String]("iv.origin")

    val (ivSuccessUrl: String, ivFailureUrl: String) = {
      val useRelativeUrls                          = platformHost.isDefined
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

  val taxCheckGuidanceUrl: String          = config.get[String]("external-url.tax-check-guidance")
  val registerForSaUrl: String             = config.get[String]("external-url.register-for-sa")
  val contactHmrcSa: String                = config.get[String]("external-url.contact-hmrc-sa")
  val companiesHouseSearchUrl: String      = config.get[String]("external-url.companies-house-search")
  val companiesHouseUpdatesUrl: String     = config.get[String]("external-url.companies-house-updates")
  val registerForCtUrl: String             = config.get[String]("external-url.register-for-ct")
  val accountingPeriodsGuidanceUrl: String = config.get[String]("external-url.accounting-periods-guidance")
  val findLostUtrUrl: String               = config.get[String]("external-url.find-lost-utr")

  val firstPageBackUrl: String = config.get[String]("first-page-back-url")

  val maxCtutrAnswerAttempts: Int = config.get[Int]("maximum-ctutr-answer-attempts")
}
