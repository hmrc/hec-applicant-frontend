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

package uk.gov.hmrc.hecapplicantfrontend.controllers

import cats.data.Validated.Valid
import cats.data.{EitherT, Validated}
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.traverse._
import uk.gov.hmrc.emailaddress.{EmailAddress => EmailAdd}
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, Call, MessagesControllerComponents, Request, Result}
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolments}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.hecapplicantfrontend.config.{AppConfig, EnrolmentConfig}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthWithRetrievalsAction
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed.AuthenticationDetails
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{AuthenticationStatus, CitizenDetails, EmailAddress, EntityType, Error, LoginData, RetrievedGGData}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, CitizenDetailsService, JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.frontend.controller.FrontendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartController @Inject() (
  appConfig: AppConfig,
  citizenDetailsService: CitizenDetailsService,
  taxCheckService: TaxCheckService,
  journeyService: JourneyService,
  auditService: AuditService,
  sessionStore: SessionStore,
  authWithRetrievalsAction: AuthWithRetrievalsAction,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with Logging {

  import StartController._

  val start: Action[AnyContent] = authWithRetrievalsAction.async { implicit request =>
    val result = for {
      maybeStoredSession                <- sessionStore.get().leftMap(BackendError)
      details                           <-
        maybeStoredSession.fold(
          handleNoSessionData(request.retrievedGGUserData)
            .map[(Option[AuthenticationDetails], LoginData)] { case (authDetails, loginData) =>
              Some(authDetails) -> loginData
            }
        )(storedSession => EitherT.pure(None -> storedSession.fold(_.loginData, _.loginData)))
      (authenticationDetails, loginData) = details
      existingTaxChecks                 <- taxCheckService
                                             .getUnexpiredTaxCheckCodes()
                                             .leftMap(BackendError(_): StartError)
      isConfirmedDetails                 = maybeStoredSession.fold(false)(_.fold(_.hasConfirmedDetails, _ => false))
      newSession                         = loginData match {
                                             case i: IndividualLoginData =>
                                               IndividualHECSession
                                                 .newSession(i)
                                                 .copy(unexpiredTaxChecks = existingTaxChecks, hasConfirmedDetails = isConfirmedDetails)
                                             case c: CompanyLoginData    =>
                                               CompanyHECSession.newSession(c).copy(unexpiredTaxChecks = existingTaxChecks)

                                           }
      _                                 <- sessionStore.store(newSession).leftMap(BackendError(_): StartError)
    } yield authenticationDetails -> newSession

    result.fold(
      {
        case DataError(msg) =>
          sys.error(s"Issue with data: $msg")

        case BackendError(e) =>
          sys.error(s"Backend error :: $e")

        case InsufficientConfidenceLevel(authDetails) =>
          auditLogIn(Some(appConfig.redirectToIvUpliftUrl), authDetails)
          appConfig.redirectToIvUpliftResult

        case UnsupportedAuthProvider(authDetails) =>
          if (authDetails.authenticationProvider === "Verify")
            auditLoginAndRedirect(routes.VerifyController.verifyNotSupported, authDetails)
          else {
            auditLogIn(None, authDetails)
            sys.error(s"Unsupported auth provider: ${authDetails.authenticationProvider}")
          }

        case AgentLogin(authDetails) =>
          auditLoginAndRedirect(routes.AgentsController.agentsNotSupported, authDetails)

      },
      { case (authDetails, session) =>
        val firstPage = journeyService.firstPage(session)
        authDetails.foreach(auditLogIn(Some(firstPage.url), _))
        Redirect(firstPage)
      }
    )
  }

  private def auditLogIn(redirectUrl: Option[String], authenticationDetails: AuthenticationDetails)(implicit
    r: Request[_],
    hc: HeaderCarrier
  ): Unit =
    auditService.sendEvent(
      ApplicantServiceStartEndPointAccessed(
        AuthenticationStatus.Authenticated,
        redirectUrl,
        Some(authenticationDetails)
      )
    )

  private def auditLoginAndRedirect(redirectTo: Call, authenticationDetails: AuthenticationDetails)(implicit
    r: Request[_],
    hc: HeaderCarrier
  ): Result = {
    auditLogIn(Some(redirectTo.url), authenticationDetails)
    Redirect(redirectTo)
  }

  private def handleNoSessionData(
    retrievedGGData: RetrievedGGData
  )(implicit request: Request[_]): EitherT[Future, StartError, (AuthenticationDetails, LoginData)] = {
    val RetrievedGGData(cl, affinityGroup, maybeNino, maybeSautr, maybeEmail, enrolments, creds) =
      retrievedGGData

    def authenticationDetails(
      affinityGroup: AffinityGroup,
      entityType: Option[EntityType]
    )(credentials: Credentials): AuthenticationDetails =
      AuthenticationDetails(
        credentials.providerType,
        credentials.providerId,
        affinityGroup,
        entityType,
        cl
      )

    affinityGroup match {
      case Some(AffinityGroup.Individual) =>
        EitherT
          .fromEither[Future](
            withGGCredIdAndAuthenticationDetails(
              creds,
              authenticationDetails(AffinityGroup.Individual, Some(EntityType.Individual))
            )
          )
          .flatMap { case (ggCredId, authDetails) =>
            handleIndividual(cl, maybeNino, maybeSautr, maybeEmail, ggCredId, authDetails)
          }

      case Some(AffinityGroup.Organisation) =>
        enrolments.enrolments.toList.filter(_.key =!= EnrolmentConfig.NINOEnrolment.key) match {
          case enrolment :: Nil if enrolment.key === EnrolmentConfig.SAEnrolment.key =>
            EitherT
              .fromEither[Future](
                withGGCredIdAndAuthenticationDetails(
                  creds,
                  authenticationDetails(AffinityGroup.Organisation, Some(EntityType.Individual))
                )
              )
              .flatMap { case (ggCredId, authDetails) =>
                handleIndividual(cl, maybeNino, maybeSautr, maybeEmail, ggCredId, authDetails)
              }

          case _ =>
            EitherT
              .fromEither[Future](
                withGGCredIdAndAuthenticationDetails(
                  creds,
                  authenticationDetails(AffinityGroup.Organisation, Some(EntityType.Company))
                )
              )
              .flatMap { case (ggCredId, authDetails) =>
                handleOrganisation(maybeEmail, enrolments, ggCredId, authDetails)
              }
        }

      case Some(AffinityGroup.Agent) =>
        EitherT
          .fromEither[Future](
            withGGCredIdAndAuthenticationDetails(
              creds,
              authenticationDetails(AffinityGroup.Agent, None)
            )
          )
          .flatMap { case (_, authDetails) =>
            EitherT.leftT(AgentLogin(authDetails))

          }

      case other =>
        EitherT.leftT(DataError(s"Unknown affinity group '${other.getOrElse("")}''"))
    }
  }

  private def handleIndividual(
    confidenceLevel: ConfidenceLevel,
    maybeNino: Option[String],
    maybeSautr: Option[String],
    maybeEmail: Option[String],
    ggCredId: GGCredId,
    authenticationDetails: AuthenticationDetails
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, StartError, (AuthenticationDetails, LoginData)] = {

    def validateSautrAndBuildIndividualData(
      citizenDetails: CitizenDetails,
      nino: String
    ): EitherT[Future, StartError, LoginData] = {
      val sautrValidation = citizenDetails.sautr match {
        case Some(s) => Valid(Some(s))
        case None    =>
          maybeSautr
            .map(SAUTR.fromString(_).toValid("Got invalid SAUTR from GG"))
            .sequence[Validated[String, *], SAUTR]
      }

      val eitherResult: Either[StartError, LoginData] = sautrValidation
        .bimap[StartError, LoginData](
          DataError,
          sautr =>
            IndividualLoginData(
              GGCredId(ggCredId.value),
              NINO(nino),
              sautr,
              citizenDetails.name,
              citizenDetails.dateOfBirth,
              validateEmail(maybeEmail)
            )
        )
        .toEither
      EitherT.fromEither[Future](eitherResult)
    }

    if (confidenceLevel < ConfidenceLevel.L250)
      EitherT.leftT(InsufficientConfidenceLevel(authenticationDetails))
    else {
      maybeNino match {
        case None =>
          EitherT.leftT(DataError("Could not find NINO for CL≥250"))

        case Some(nino) if Nino.isValid(nino) =>
          val citizenDetailsFut = citizenDetailsService
            .getCitizenDetails(NINO(nino))
            .leftMap(BackendError(_): StartError)

          for {
            citizenDetails <- citizenDetailsFut
            result         <- validateSautrAndBuildIndividualData(citizenDetails, nino)
          } yield authenticationDetails -> result

        case Some(_)                          =>
          EitherT.leftT(DataError("Invalid NINO format"))
      }
    }
  }

  private def handleOrganisation(
    maybeEmail: Option[String],
    enrolments: Enrolments,
    ggCredId: GGCredId,
    authenticationDetails: AuthenticationDetails
  ): EitherT[Future, StartError, (AuthenticationDetails, LoginData)] = {
    val ctutrValidation = enrolments.enrolments
      .find(_.key === EnrolmentConfig.CTEnrolment.key)
      .flatMap(
        _.getIdentifier(EnrolmentConfig.CTEnrolment.ctutrIdentifier).map(id =>
          CTUTR.fromString(id.value).toValid("Got invalid CTUTR from enrolments")
        )
      )
      .sequence[Validated[String, *], CTUTR]

    val eitherResult = ctutrValidation
      .bimap[StartError, LoginData](
        DataError,
        ctutrOpt => CompanyLoginData(GGCredId(ggCredId.value), ctutrOpt, validateEmail(maybeEmail))
      )
      .toEither

    EitherT.fromEither[Future](eitherResult.map(authenticationDetails -> _))
  }

  private def validateEmail(emailOpt: Option[String]): Option[EmailAddress] =
    emailOpt.filter(EmailAdd.isValid(_)).map(EmailAddress(_))

  private def withGGCredIdAndAuthenticationDetails(
    credentials: Option[Credentials],
    toAuthenticationDetails: Credentials => AuthenticationDetails
  ): Either[StartError, (GGCredId, AuthenticationDetails)] =
    credentials match {
      case None =>
        Left(DataError("No credentials were retrieved"))

      case Some(c @ Credentials(id, "GovernmentGateway")) =>
        val ggCredId = GGCredId(id)
        Right(ggCredId -> toAuthenticationDetails(c))

      case Some(other)                                    =>
        Left(UnsupportedAuthProvider(toAuthenticationDetails(other)))
    }

}

object StartController {

  sealed trait StartError extends Product with Serializable

  final case class DataError(msg: String) extends StartError

  final case class BackendError(error: Error) extends StartError

  final case class InsufficientConfidenceLevel(authenticationDetails: AuthenticationDetails) extends StartError

  final case class UnsupportedAuthProvider(authenticationDetails: AuthenticationDetails) extends StartError

  final case class AgentLogin(authenticationDetails: AuthenticationDetails) extends StartError

}
