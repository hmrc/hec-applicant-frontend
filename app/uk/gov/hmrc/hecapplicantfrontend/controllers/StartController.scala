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

import cats.data.Validated.Valid
import cats.data.{EitherT, Validated}
import cats.instances.future._
import cats.instances.option._
import cats.instances.string._
import cats.syntax.eq._
import cats.syntax.option._
import cats.syntax.traverse._
import com.google.inject.{Inject, Singleton}
import play.api.mvc.{Action, AnyContent, MessagesControllerComponents, Request}
import uk.gov.hmrc.auth.core.retrieve.Credentials
import uk.gov.hmrc.auth.core.{AffinityGroup, ConfidenceLevel, Enrolments}
import uk.gov.hmrc.domain.Nino
import uk.gov.hmrc.hecapplicantfrontend.config.{AppConfig, EnrolmentConfig}
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.AuthWithRetrievalsAction
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{CitizenDetails, EmailAddress, Error, LoginData, RetrievedGGData}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{CitizenDetailsService, JourneyService, TaxCheckService}
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
  sessionStore: SessionStore,
  authWithRetrievalsAction: AuthWithRetrievalsAction,
  mcc: MessagesControllerComponents
)(implicit ec: ExecutionContext)
    extends FrontendController(mcc)
    with Logging {

  import StartController._

  val start: Action[AnyContent] = authWithRetrievalsAction.async { implicit request =>
    val result = for {
      maybeStoredSession           <- sessionStore.get().leftMap(BackendError)
      loginData                    <- maybeStoredSession.fold(handleNoSessionData(request.retrievedGGUserData))(storedSession =>
                                        EitherT.pure(storedSession.fold(_.loginData, _.loginData))
                                      )
      existingTaxChecks            <- taxCheckService
                                        .getUnexpiredTaxCheckCodes()
                                        .leftMap(BackendError(_): StartError)
      hasConfirmedIndividualDetails = maybeStoredSession.flatMap(_.fold(_.hasConfirmedDetails, _ => None))
      newSession                    = loginData match {
                                        case i: IndividualLoginData =>
                                          IndividualHECSession
                                            .newSession(i)
                                            .copy(
                                              unexpiredTaxChecks = existingTaxChecks,
                                              hasConfirmedDetails = hasConfirmedIndividualDetails
                                            )
                                        case c: CompanyLoginData    =>
                                          CompanyHECSession.newSession(c).copy(unexpiredTaxChecks = existingTaxChecks)

                                      }
      _                            <- sessionStore.store(newSession).leftMap(BackendError(_): StartError)
    } yield newSession

    result.fold(
      {
        case DataError(msg) =>
          sys.error(s"Issue with data: $msg")

        case BackendError(e) =>
          sys.error(s"Backend error :: $e")

        case InsufficientConfidenceLevel =>
          appConfig.redirectToIvUplift

        case UnsupportedAuthProvider(provider) =>
          sys.error(s"Unsupported auth provider: $provider")

        case AgentLogin =>
          sys.error("Agent login")

      },
      session => Redirect(journeyService.firstPage(session))
    )
  }

  private def handleNoSessionData(
    retrievedGGData: RetrievedGGData
  )(implicit request: Request[_]): EitherT[Future, StartError, LoginData] = {
    val RetrievedGGData(cl, affinityGroup, maybeNino, maybeSautr, maybeEmail, enrolments, creds) =
      retrievedGGData

    withGGCredentials(creds) { ggCredId =>
      affinityGroup match {
        case Some(AffinityGroup.Individual) =>
          handleIndividual(
            cl,
            maybeNino,
            maybeSautr,
            maybeEmail,
            ggCredId
          )

        case Some(AffinityGroup.Organisation) =>
          enrolments.enrolments.toList.filter(_.key =!= EnrolmentConfig.NINOEnrolment.key) match {
            case enrolment :: Nil if enrolment.key === EnrolmentConfig.SAEnrolment.key =>
              handleIndividual(
                cl,
                maybeNino,
                maybeSautr,
                maybeEmail,
                ggCredId
              )

            case _ =>
              handleOrganisation(maybeEmail, enrolments, ggCredId)
          }

        case Some(AffinityGroup.Agent) =>
          EitherT.leftT(AgentLogin)

        case other =>
          EitherT.leftT(DataError(s"Unknown affinity group '${other.getOrElse("")}''"))
      }
    }
  }

  private def handleIndividual(
    confidenceLevel: ConfidenceLevel,
    maybeNino: Option[String],
    maybeSautr: Option[String],
    maybeEmail: Option[String],
    ggCredId: GGCredId
  )(implicit
    hc: HeaderCarrier
  ): EitherT[Future, StartError, LoginData] = {

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
              maybeEmail.map(EmailAddress(_))
            )
        )
        .toEither
      EitherT.fromEither[Future](eitherResult)
    }

    if (confidenceLevel < ConfidenceLevel.L250)
      EitherT.leftT(InsufficientConfidenceLevel)
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
          } yield result

        case Some(_) =>
          EitherT.leftT(DataError("Invalid NINO format"))
      }
    }
  }

  private def handleOrganisation(
    maybeEmail: Option[String],
    enrolments: Enrolments,
    ggCredId: GGCredId
  ): EitherT[Future, StartError, LoginData] = {
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
        ctutrOpt => CompanyLoginData(GGCredId(ggCredId.value), ctutrOpt, maybeEmail.map(EmailAddress(_)))
      )
      .toEither

    EitherT.fromEither[Future](eitherResult)
  }

  private def withGGCredentials[A](
    credentials: Option[Credentials]
  )(
    f: GGCredId => EitherT[Future, StartError, A]
  ): EitherT[Future, StartError, A] =
    credentials match {
      case None =>
        EitherT.leftT(DataError("No credentials were retrieved"))

      case Some(Credentials(id, "GovernmentGateway")) =>
        f(GGCredId(id))

      case Some(Credentials(_, otherProvider)) =>
        EitherT.leftT(UnsupportedAuthProvider(otherProvider))
    }
}

object StartController {

  sealed trait StartError extends Product with Serializable

  final case class DataError(msg: String) extends StartError

  final case class BackendError(error: Error) extends StartError

  final case object InsufficientConfidenceLevel extends StartError

  final case class UnsupportedAuthProvider(provider: String) extends StartError

  final case object AgentLogin extends StartError

}
