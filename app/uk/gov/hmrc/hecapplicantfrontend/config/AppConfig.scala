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

package uk.gov.hmrc.hecapplicantfrontend.config

import cats.instances.char._
import cats.syntax.eq._
import play.api.Configuration
import play.api.mvc.{Call, Result}
import play.api.mvc.Results.Redirect
import uk.gov.hmrc.auth.core.ConfidenceLevel
import uk.gov.hmrc.hecapplicantfrontend.controllers.routes
import uk.gov.hmrc.hecapplicantfrontend.models.EntityType
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.hmrcfrontend.config.ContactFrontendConfig

import java.util.UUID
import javax.inject.{Inject, Singleton}
import scala.concurrent.duration.FiniteDuration

@Singleton
class AppConfig @Inject() (config: Configuration, contactFrontendConfig: ContactFrontendConfig) {

  val platformHost: Option[String] = config.getOptional[String]("platform.frontend.host")

  val contactFrontendUrl: String =
    contactFrontendConfig.baseUrl.getOrElse(sys.error("Could not find config for contact frontend url"))

  val contactFormServiceIdentifier: String =
    contactFrontendConfig.serviceId.getOrElse(sys.error("Could not find config for contact frontend service id"))

  val betaFeedbackUrl: String = s"$contactFrontendUrl/contact/beta-feedback?service=$contactFormServiceIdentifier"

  val welshLanguageSupportEnabled: Boolean =
    config.getOptional[Boolean]("features.welsh-language-support").getOrElse(false)

  val selfBaseUrl: String = platformHost.getOrElse(config.get[String]("self.url"))

  val ggOrigin: String = config.get[String]("auth.gg.origin")

  def signInUrl(continue: Call): String = {
    val url: String = config.get[String]("auth.sign-in.url")
    s"$url?continue=${(s"$selfBaseUrl${continue.url}").urlEncode}&origin=$ggOrigin"
  }

  private val signOutUrlBase: String = config.get[String]("auth.sign-out.url")

  def signOutUrl(continueUrl: Option[String]): String =
    continueUrl.fold(signOutUrlBase)(continue => s"$signOutUrlBase?continue=${continue.urlEncode}")

  lazy val signOutAndSignBackInUrl: String =
    signOutUrl(continueUrl = Some(s"$selfBaseUrl${routes.StartController.start.url}"))

  private val registerForNewGGAccountUrl: String = config.get[String]("auth.register-new-account.url")

  def registerForNewGGAccountUrl(entityType: EntityType): String = {
    val accountType = entityType match {
      case EntityType.Individual => "Individual"
      case EntityType.Company    => "Organisation"
    }
    s"$registerForNewGGAccountUrl?continueUrl=${routes.StartController.start.url.urlEncode}&accountType=$accountType&origin=$ggOrigin"
  }

  val authTimeoutSeconds: Int = config.get[FiniteDuration]("auth.sign-out.inactivity-timeout").toSeconds.toInt

  val authTimeoutCountdownSeconds: Int =
    config.get[FiniteDuration]("auth.sign-out.inactivity-countdown").toSeconds.toInt

  private val ivUrl: String             = platformHost.getOrElse(config.get[String]("iv.url"))
  private val ivLocation                = config.get[String]("iv.location")
  private val ivOrigin: String          = config.get[String]("iv.origin")
  private val ivUseRelativeUrls         = platformHost.isDefined
  private lazy val ivFailureRelativeUrl = routes.IvFailureController.ivFailure(UUID.randomUUID).url.takeWhile(_ =!= '?')
  private val redirectToIvUrl: String   = s"$ivUrl$ivLocation/uplift"

  def redirectToIvUpliftUrlWithResult(successContinue: Call): (String, Result) = {
    val successRelativeUrl = successContinue.url

    val (ivSuccessUrl: String, ivFailureUrl: String) =
      if (ivUseRelativeUrls)
        successRelativeUrl                 -> ivFailureRelativeUrl
      else
        s"$selfBaseUrl$successRelativeUrl" -> s"$selfBaseUrl$ivFailureRelativeUrl"

    redirectToIvUrl -> Redirect(
      redirectToIvUrl,
      Map(
        "origin"          -> Seq(ivOrigin),
        "confidenceLevel" -> Seq(ConfidenceLevel.L250.level.toString),
        "completionURL"   -> Seq(ivSuccessUrl),
        "failureURL"      -> Seq(ivFailureUrl)
      )
    )
  }

  val applicantServiceGuidanceUrl: String  = config.get[String]("external-url.applicant-service-guidance")
  val taxCheckGuidanceUrl: String          = config.get[String]("external-url.tax-check-guidance")
  val registerForSaUrl: String             = config.get[String]("external-url.register-for-sa")
  val contactHmrcSa: String                = config.get[String]("external-url.contact-hmrc-sa")
  val companiesHouseSearchUrl: String      = config.get[String]("external-url.companies-house-search")
  val companiesHouseUpdatesUrl: String     = config.get[String]("external-url.companies-house-updates")
  val registerForCtUrl: String             = config.get[String]("external-url.register-for-ct")
  val accountingPeriodsGuidanceUrl: String = config.get[String]("external-url.accounting-periods-guidance")
  val findLostUtrUrl: String               = config.get[String]("external-url.find-lost-utr")
  val saGuidanceUrl: String                = config.get[String]("external-url.sa-guidance")

  val maxCtutrAnswerAttempts: Int = config.get[Int]("ctutr-attempts.maximum-attempts")

  val maxTaxChecksPerLicenceType: Int = config.get[Int]("max-tax-checks-per-licence-type")

  val exitSurveyUrl: String = {
    val baseUrl = platformHost.getOrElse(config.get[String]("feedback-frontend.base-url"))
    s"$baseUrl/feedback/$contactFormServiceIdentifier"
  }

  val userRecruitmentUrl: String = config.get[String]("external-url.user-recruitment")

}
