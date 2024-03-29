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

import cats.data.EitherT
import cats.implicits.catsSyntaxOptionId
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.{Cookie, Request, Result}
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.hecapplicantfrontend.config.EnrolmentConfig
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed
import uk.gov.hmrc.hecapplicantfrontend.models.AuditEvent.ApplicantServiceStartEndPointAccessed.AuthenticationDetails
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{AuthenticationStatus, CitizenDetails, DateOfBirth, EmailAddress, EntityType, Error, HECSession, Language, Name, TaxCheckListItem, UncertainEntityTypeJourney}
import uk.gov.hmrc.hecapplicantfrontend.repos.{SessionStore, UncertainEntityTypeJourneyStore}
import uk.gov.hmrc.hecapplicantfrontend.services.{AuditService, AuditServiceSupport, CitizenDetailsService, JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.http.HeaderCarrier

import java.net.URLEncoder
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StartControllerSpec
    extends ControllerSpec
    with AuthSupport
    with SessionSupport
    with JourneyServiceSupport
    with AuditServiceSupport {

  import StartControllerSpec._

  val ivOrigin    = "ivOrigin"
  val ivUrl       = "https://iv:123"
  val ivLocation  = "/iv-location"
  val selfBaseUrl = "http://self:456"
  val signInUrl   = "https://sign-in:456"
  val ggOrigin    = "ggOrigin"

  override lazy val additionalConfig: Configuration = Configuration(
    ConfigFactory.parseString(
      s"""
         |iv {
         |  origin = "$ivOrigin"
         |  location = $ivLocation
         |  url = "$ivUrl"
         |  use-relative-urls = false
         |}
         |
         |self.url = "$selfBaseUrl"
         |
         |auth {
         |   sign-in.url = "$signInUrl"
         |   gg.origin = "$ggOrigin"
         |}
         |""".stripMargin
    )
  )

  val mockCitizenDetailsService           = mock[CitizenDetailsService]
  val mockTaxCheckService                 = mock[TaxCheckService]
  val mockUncertainEntityTypeJourneyStore = mock[UncertainEntityTypeJourneyStore]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[CitizenDetailsService].toInstance(mockCitizenDetailsService),
      bind[TaxCheckService].toInstance(mockTaxCheckService),
      bind[JourneyService].toInstance(mockJourneyService),
      bind[AuditService].toInstance(mockAuditService),
      bind[UncertainEntityTypeJourneyStore].toInstance(mockUncertainEntityTypeJourneyStore)
    )

  val retrievals =
    Retrievals.confidenceLevel and Retrievals.affinityGroup and Retrievals.nino and
      Retrievals.saUtr and Retrievals.email and Retrievals.allEnrolments and Retrievals.credentials

  def mockAuthWithRetrievals(
    retrievedConfidenceLevel: ConfidenceLevel,
    retrievedAffinityGroup: Option[AffinityGroup],
    retrievedNINO: Option[NINO],
    retrievedSAUTR: Option[SAUTR],
    retrievedEmailAddress: Option[EmailAddress],
    retrievedEnrolments: Enrolments,
    retrievedCredentials: Option[Credentials]
  ) =
    mockAuth(EmptyPredicate, retrievals)(
      Future.successful(
        new ~(retrievedConfidenceLevel, retrievedAffinityGroup) and
          retrievedNINO.map(_.value) and
          retrievedSAUTR.map(_.value) and
          retrievedEmailAddress.map(_.value) and
          retrievedEnrolments and
          retrievedCredentials
      )
    )

  def mockGetCitizenDetails(nino: NINO)(result: Either[Error, Option[CitizenDetails]]) =
    (mockCitizenDetailsService
      .getCitizenDetails(_: NINO)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(EitherT.fromEither(result))

  def mockGetUnexpiredTaxCheckCodes(result: Either[Error, List[TaxCheckListItem]]) =
    (mockTaxCheckService
      .getUnexpiredTaxCheckCodes()(_: HeaderCarrier))
      .expects(*)
      .returning(EitherT.fromEither(result))

  val ggProviderType = "GovernmentGateway"

  def retrievedGGCredential(ggCredId: GGCredId) =
    Credentials(ggCredId.value, ggProviderType)

  def retrievedCtEnrolment(ctutr: CTUTR) = Enrolment(
    EnrolmentConfig.CTEnrolment.key,
    List(EnrolmentIdentifier(EnrolmentConfig.CTEnrolment.ctutrIdentifier, ctutr.value)),
    "state",
    None
  )

  def mockGetUncertainEntityTypeJourney(result: Either[Error, Option[UncertainEntityTypeJourney]]) =
    (mockUncertainEntityTypeJourneyStore
      .get()(_: Request[_]))
      .expects(*)
      .returning(EitherT.fromEither(result))

  def mockStoreUncertainEntityTypeJourney(journey: UncertainEntityTypeJourney)(result: Either[Error, Unit]) =
    (mockUncertainEntityTypeJourneyStore
      .store(_: UncertainEntityTypeJourney)(_: Request[_]))
      .expects(journey, *)
      .returning(EitherT.fromEither(result))

  val controller = instanceOf[StartController]

  "StartController" when {

    val ggCredId = GGCredId("credId")

    val sautr = SAUTR("1234567895")

    val emailAddress = EmailAddress("user@test.com")

    val ctutr = CTUTR("1234567895")

    val nino = NINO("AB123456C")

    val completeIndividualLoginData =
      IndividualLoginData(
        ggCredId,
        nino,
        Some(sautr),
        Name("First", "Last"),
        DateOfBirth(LocalDate.now()),
        Some(emailAddress),
        Some(false)
      )

    val completeCompanyLoginData = CompanyLoginData(
      ggCredId,
      Some(ctutr),
      Some(emailAddress),
      Some(false)
    )

    val enrolmentCombinationsForIndividuals =
      List(
        Set(Enrolment(EnrolmentConfig.SAEnrolment.key)),
        Set(Enrolment(EnrolmentConfig.MTDITEnrolment.key)),
        Set(
          Enrolment(EnrolmentConfig.SAEnrolment.key),
          Enrolment(EnrolmentConfig.MTDITEnrolment.key)
        ),
        Set(
          Enrolment(EnrolmentConfig.SAEnrolment.key),
          Enrolment(EnrolmentConfig.NINOEnrolment.key)
        ),
        Set(
          Enrolment(EnrolmentConfig.MTDITEnrolment.key),
          Enrolment(EnrolmentConfig.NINOEnrolment.key)
        ),
        Set(
          Enrolment(EnrolmentConfig.SAEnrolment.key),
          Enrolment(EnrolmentConfig.MTDITEnrolment.key),
          Enrolment(EnrolmentConfig.NINOEnrolment.key)
        )
      )

    "handling requests to the start endpoint" must {

      def performAction(languageCode: String = Language.English.code): Future[Result] =
        controller.start(FakeRequest().withCookies(Cookie("PLAY_LANG", languageCode)))

      "proceed" when {

        "existing session data is found" when {

          def testIsRedirect(
            session: HECSession,
            affinityGroup: Option[AffinityGroup],
            retrievedEmailAddress: Option[EmailAddress],
            retrievedEnrolments: Enrolments,
            retrievedCredentials: Option[Credentials]
          ) = {
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                affinityGroup,
                None,
                None,
                retrievedEmailAddress,
                retrievedEnrolments,
                retrievedCredentials
              )
              mockGetSession(session)
              mockGetUnexpiredTaxCheckCodes(Right(List.empty))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
            }
            checkIsRedirect(performAction(), mockNextCall)
          }

          "it's individual session and hasConfirmedDetails = false, new session should have it as false" in {
            val session = IndividualHECSession.newSession(completeIndividualLoginData)
            testIsRedirect(session, None, None, Enrolments(Set.empty), None)
          }

          "it's individual session and hasConfirmedDetails = true, new session should have it as true" in {
            val tempSession = IndividualHECSession.newSession(completeIndividualLoginData)
            val session     = tempSession.copy(hasConfirmedDetails = true)
            testIsRedirect(session, None, None, Enrolments(Set.empty), None)
          }

          "it's company session" in {
            val session = CompanyHECSession.newSession(completeCompanyLoginData)
            testIsRedirect(
              session,
              Some(AffinityGroup.Organisation),
              emailAddress.some,
              Enrolments(Set(retrievedCtEnrolment(ctutr))),
              Some(retrievedGGCredential(ggCredId))
            )
          }

        }

        "no session data is found and" when {

          def isRedirectTest(
            session: HECSession,
            individualLoginData: IndividualLoginData,
            citizenDetails: CitizenDetails,
            sautr: Option[SAUTR],
            affinityGroup: AffinityGroup,
            enrolments: Enrolments,
            mockGetUncertainEntityTypeJourney: Option[() => Unit] = None
          ) = {
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(affinityGroup),
                Some(individualLoginData.nino),
                sautr,
                individualLoginData.emailAddress,
                enrolments,
                Some(retrievedGGCredential(individualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetUncertainEntityTypeJourney.foreach(_())
              mockGetCitizenDetails(individualLoginData.nino)(Right(citizenDetails.some))
              mockGetUnexpiredTaxCheckCodes(Right(List.empty))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
              mockSendAuditEvent(
                ApplicantServiceStartEndPointAccessed(
                  AuthenticationStatus.Authenticated,
                  Some(mockNextCall.url),
                  Some(
                    AuthenticationDetails(
                      ggProviderType,
                      ggCredId.value,
                      Some(affinityGroup),
                      Some(EntityType.Individual),
                      ConfidenceLevel.L250
                    )
                  )
                )
              )
            }

            checkIsRedirect(performAction(), mockNextCall)
          }

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Individual' and CL250 and email address is valid" in {
              List(
                completeIndividualLoginData,
                completeIndividualLoginData.copy(emailAddress = None),
                completeIndividualLoginData.copy(sautr = None)
              ).foreach { individualRetrievedData =>
                val citizenDetails = CitizenDetails(
                  individualRetrievedData.name,
                  individualRetrievedData.dateOfBirth,
                  individualRetrievedData.sautr
                )

                val session =
                  IndividualHECSession.newSession(individualRetrievedData)
                isRedirectTest(
                  session,
                  individualRetrievedData,
                  citizenDetails,
                  None,
                  AffinityGroup.Individual,
                  Enrolments(Set.empty)
                )
              }
            }

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Individual' and CL250 and  email address is not  valid" in {
              val individualRetrievedData = completeIndividualLoginData.copy(emailAddress = EmailAddress("email").some)
              val citizenDetails          = CitizenDetails(
                individualRetrievedData.name,
                individualRetrievedData.dateOfBirth,
                individualRetrievedData.sautr
              )

              val loginData = IndividualHECSession.newSession(individualRetrievedData).loginData
              val session   = IndividualHECSession
                .newSession(individualRetrievedData)
                .copy(loginData = loginData.copy(emailAddress = None))
              isRedirectTest(
                session,
                individualRetrievedData,
                citizenDetails,
                None,
                AffinityGroup.Individual,
                Enrolments(Set.empty)
              )

            }

          "no SAUTR is returned in citizen details but one is retrieved from the GG cred" in {
            val citizenDetails = CitizenDetails(
              completeIndividualLoginData.name,
              completeIndividualLoginData.dateOfBirth,
              None
            )

            val session =
              IndividualHECSession.newSession(completeIndividualLoginData)
            isRedirectTest(
              session,
              completeIndividualLoginData,
              citizenDetails,
              sautr.some,
              AffinityGroup.Individual,
              Enrolments(Set.empty)
            )
          }

          "an SAUTR is retrieved in the GG cred and in citizen details" in {
            val citizenDetailsSautr = SAUTR("citizen-details-sautr")
            val ggSautr             = SAUTR("gg-sautr")

            val citizenDetails = CitizenDetails(
              completeIndividualLoginData.name,
              completeIndividualLoginData.dateOfBirth,
              Some(citizenDetailsSautr)
            )

            val session =
              IndividualHECSession
                .newSession(completeIndividualLoginData.copy(sautr = Some(citizenDetailsSautr)))

            isRedirectTest(
              session,
              completeIndividualLoginData,
              citizenDetails,
              ggSautr.some,
              AffinityGroup.Individual,
              Enrolments(Set.empty)
            )

          }

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Organisation' and " when {

              val citizenDetails = CitizenDetails(
                completeIndividualLoginData.name,
                completeIndividualLoginData.dateOfBirth,
                completeIndividualLoginData.sautr
              )

              val session =
                IndividualHECSession
                  .newSession(completeIndividualLoginData)

              "the enrolments indicate the user is an individual and the user has CL250" in {
                enrolmentCombinationsForIndividuals.foreach { enrolments =>
                  withClue(s"For enrolments $enrolments: ") {
                    isRedirectTest(
                      session,
                      completeIndividualLoginData,
                      citizenDetails,
                      None,
                      AffinityGroup.Organisation,
                      Enrolments(enrolments)
                    )
                  }
                }
              }

              "it is not clear from the enrolments and the CL what the entity type is but the user has clarified that they are " +
                "an individual" in {
                  val loginData = completeIndividualLoginData.copy(didConfirmUncertainEntityType = Some(true))
                  isRedirectTest(
                    session.copy(loginData = loginData),
                    loginData,
                    citizenDetails,
                    None,
                    AffinityGroup.Organisation,
                    Enrolments(Set.empty),
                    Some(() =>
                      mockGetUncertainEntityTypeJourney(
                        Right(
                          Some(
                            UncertainEntityTypeJourney(
                              loginData.ggCredId,
                              Some(EntityType.Individual)
                            )
                          )
                        )
                      )
                    )
                  )
                }

            }

          "all the necessary data is retrieved for a company" when {

            "it is clear from the enrolments and the confidence level that the user is a company" in {
              List(
                completeCompanyLoginData,
                completeCompanyLoginData.copy(emailAddress = None)
              ).foreach { companyRetrievedData =>
                val session = CompanyHECSession.newSession(companyRetrievedData)
                inSequence {
                  mockAuthWithRetrievals(
                    ConfidenceLevel.L50,
                    Some(AffinityGroup.Organisation),
                    None,
                    None,
                    companyRetrievedData.emailAddress,
                    Enrolments(Set(retrievedCtEnrolment(ctutr))),
                    Some(retrievedGGCredential(companyRetrievedData.ggCredId))
                  )
                  mockGetSession(Right(None))
                  mockGetUnexpiredTaxCheckCodes(Right(List.empty))
                  mockStoreSession(session)(Right(()))
                  mockFirstPge(session)(mockNextCall)
                  mockSendAuditEvent(
                    ApplicantServiceStartEndPointAccessed(
                      AuthenticationStatus.Authenticated,
                      Some(mockNextCall.url),
                      Some(
                        AuthenticationDetails(
                          ggProviderType,
                          ggCredId.value,
                          Some(AffinityGroup.Organisation),
                          Some(EntityType.Company),
                          ConfidenceLevel.L50
                        )
                      )
                    )
                  )
                }

                checkIsRedirect(performAction(), mockNextCall)
              }
            }

            "it is not clear from the enrolments and the confidence level that the user is a company but " +
              "the user has clarified that they are a company" in {
                val loginData = completeCompanyLoginData.copy(didConfirmUncertainEntityType = Some(true))
                val session   =
                  CompanyHECSession.newSession(loginData)
                inSequence {
                  mockAuthWithRetrievals(
                    ConfidenceLevel.L250,
                    Some(AffinityGroup.Organisation),
                    None,
                    None,
                    loginData.emailAddress,
                    Enrolments(Set(retrievedCtEnrolment(ctutr))),
                    Some(retrievedGGCredential(loginData.ggCredId))
                  )
                  mockGetSession(Right(None))
                  mockGetUncertainEntityTypeJourney(
                    Right(
                      Some(
                        UncertainEntityTypeJourney(
                          loginData.ggCredId,
                          Some(EntityType.Company)
                        )
                      )
                    )
                  )
                  mockGetUnexpiredTaxCheckCodes(Right(List.empty))
                  mockStoreSession(session)(Right(()))
                  mockFirstPge(session)(mockNextCall)
                  mockSendAuditEvent(
                    ApplicantServiceStartEndPointAccessed(
                      AuthenticationStatus.Authenticated,
                      Some(mockNextCall.url),
                      Some(
                        AuthenticationDetails(
                          ggProviderType,
                          loginData.ggCredId.value,
                          Some(AffinityGroup.Organisation),
                          Some(EntityType.Company),
                          ConfidenceLevel.L250
                        )
                      )
                    )
                  )
                }

                checkIsRedirect(performAction(), mockNextCall)
              }

          }

          "no CTUTR can be found for a company" in {
            val companyData = completeCompanyLoginData.copy(ctutr = None)
            val session     = CompanyHECSession.newSession(companyData)
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                completeCompanyLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeCompanyLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetUnexpiredTaxCheckCodes(Right(List.empty))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
              mockSendAuditEvent(
                ApplicantServiceStartEndPointAccessed(
                  AuthenticationStatus.Authenticated,
                  Some(mockNextCall.url),
                  Some(
                    AuthenticationDetails(
                      ggProviderType,
                      ggCredId.value,
                      Some(AffinityGroup.Organisation),
                      Some(EntityType.Company),
                      ConfidenceLevel.L50
                    )
                  )
                )
              )

            }

            checkIsRedirect(performAction(), mockNextCall)
          }

          "it is not clear from the enrolments and the confidence what the entity type is and the user has not " +
            "clarified which entity type they are" in {
              val journey =
                UncertainEntityTypeJourney(ggCredId, None)

              val next = routes.ConfirmUncertainEntityTypeController.entityType

              List(
                Some(journey) -> None,
                None          -> Some(() => mockStoreUncertainEntityTypeJourney(journey)(Right(())))
              ).foreach { case (existingJourney, mockStoreJourney) =>
                inSequence {
                  mockAuthWithRetrievals(
                    ConfidenceLevel.L250,
                    Some(AffinityGroup.Organisation),
                    None,
                    None,
                    Some(emailAddress),
                    Enrolments(Set.empty),
                    Some(retrievedGGCredential(ggCredId))
                  )
                  mockGetSession(Right(None))
                  mockGetUncertainEntityTypeJourney(Right(existingJourney))
                  mockStoreJourney.foreach(_())
                  mockSendAuditEvent(
                    ApplicantServiceStartEndPointAccessed(
                      AuthenticationStatus.Authenticated,
                      Some(next.url),
                      Some(
                        AuthenticationDetails(
                          ggProviderType,
                          ggCredId.value,
                          Some(AffinityGroup.Organisation),
                          None,
                          ConfidenceLevel.L250
                        )
                      )
                    )
                  )

                }

                checkIsRedirect(performAction(), next)
              }
            }

        }
      }

      "return a technical error" when {

        def testIsError(mockActions: () => Unit) = {
          mockActions()
          assertThrows[RuntimeException](await(performAction()))
        }

        "an AuthorisationException is thrown" in {
          List[AuthorisationException](
            InsufficientEnrolments(),
            UnsupportedAffinityGroup(),
            UnsupportedCredentialRole(),
            UnsupportedAuthProvider(),
            IncorrectCredentialStrength(),
            InternalError()
          ).foreach { e =>
            withClue(s"For error $e: ") {
              testIsError(() => mockAuth(EmptyPredicate, retrievals)(Future.failed(e)))
            }
          }

        }

        "there is an error getting session data" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Left(Error("")))
            }
          )
        }

        "the retrieved affinity group is empty" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                None,
                Some(completeIndividualLoginData.nino),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
            }
          )
        }

        "the retrieved credential type is not GG" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(Credentials("id", "OtherProvider"))
              )
              mockGetSession(Right(None))
              mockSendAuditEvent(
                ApplicantServiceStartEndPointAccessed(
                  AuthenticationStatus.Authenticated,
                  None,
                  Some(
                    AuthenticationDetails(
                      "OtherProvider",
                      "id",
                      Some(AffinityGroup.Individual),
                      Some(EntityType.Individual),
                      ConfidenceLevel.L50
                    )
                  )
                )
              )
            }
          )
        }

        "the retrieved credential is empty" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                None
              )
              mockGetSession(Right(None))
            }
          )
        }

        "a NINO cannot be found for a CL250 individual" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                None,
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
            }
          )
        }

        "the NINO format is invalid" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(NINO("invalid-nino")),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
            }
          )
        }

        "there is an error getting citizen details for an individual" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Left(Error("")))
            }
          )
        }

        "there is an error fetching existing tax check codes for an individual" in {
          val citizenDetails = CitizenDetails(
            completeIndividualLoginData.name,
            completeIndividualLoginData.dateOfBirth,
            None
          )

          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(sautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails.some))
              mockGetUnexpiredTaxCheckCodes(Left(Error("")))
            }
          )
        }

        "there is an error storing session data" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                completeCompanyLoginData.emailAddress,
                Enrolments(Set(retrievedCtEnrolment(ctutr))),
                Some(retrievedGGCredential(completeCompanyLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetUnexpiredTaxCheckCodes(Right(List.empty))
              mockStoreSession(
                CompanyHECSession.newSession(completeCompanyLoginData)
              )(Left(Error("")))
            }
          )
        }

        "there is no SA UTR from citizen details and there is an invalid SA UTR retrieved from GG" in {
          val citizenDetails = CitizenDetails(
            completeIndividualLoginData.name,
            completeIndividualLoginData.dateOfBirth,
            None
          )

          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(SAUTR("invalid")),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails.some))
            }
          )
        }

        "there is an invalid CT UTR in the enrolments" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                completeCompanyLoginData.emailAddress,
                Enrolments(Set(retrievedCtEnrolment(CTUTR("invalid")))),
                Some(retrievedGGCredential(completeCompanyLoginData.ggCredId))
              )
              mockGetSession(Right(None))
            }
          )
        }

        "there is an error fetching existing tax check codes for a company" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                completeCompanyLoginData.emailAddress,
                Enrolments(Set(retrievedCtEnrolment(ctutr))),
                Some(retrievedGGCredential(completeCompanyLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetUnexpiredTaxCheckCodes(Left(Error("")))
            }
          )
        }

        "there is an error retrieving from the UncertainEntityTypeJourneyStore" in {
          testIsError { () =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Organisation),
                None,
                None,
                Some(emailAddress),
                Enrolments(Set.empty),
                Some(retrievedGGCredential(ggCredId))
              )
              mockGetSession(Right(None))
              mockGetUncertainEntityTypeJourney(Left(Error("")))
            }

          }
        }

        "there is an error writing to the UncertainEntityTypeJourneyStore" in {
          testIsError { () =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Organisation),
                None,
                None,
                Some(emailAddress),
                Enrolments(Set.empty),
                Some(retrievedGGCredential(ggCredId))
              )
              mockGetSession(Right(None))
              mockGetUncertainEntityTypeJourney(Right(None))
              mockStoreUncertainEntityTypeJourney(UncertainEntityTypeJourney(ggCredId, None))(
                Left(Error(""))
              )
            }

          }
        }
      }

      "redirect to IV uplift" when {

        val queryString =
          s"origin=$ivOrigin&confidenceLevel=250&" +
            s"completionURL=${urlEncode(s"$selfBaseUrl/tax-check-for-licence/start")}&" +
            s"failureURL=${urlEncode(s"$selfBaseUrl/tax-check-for-licence/failed-iv/callback")}"

        val expectedIvUrl = s"$ivUrl$ivLocation/uplift?$queryString"

        "the user has CL<250 and" when {

          "the affinity group is 'Individual'" in {
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L200,
                Some(AffinityGroup.Individual),
                None,
                None,
                None,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(ggCredId))
              )
              mockGetSession(Right(None))
              mockSendAuditEvent(
                ApplicantServiceStartEndPointAccessed(
                  AuthenticationStatus.Authenticated,
                  Some(expectedIvUrl.stripSuffix(s"?$queryString")),
                  Some(
                    AuthenticationDetails(
                      ggProviderType,
                      ggCredId.value,
                      Some(AffinityGroup.Individual),
                      Some(EntityType.Individual),
                      ConfidenceLevel.L200
                    )
                  )
                )
              )
            }

            checkIsRedirect(performAction(), expectedIvUrl)

          }

          "the affinity group is 'Organisation' and" when {

            "it is clear from the enrolments that the user is an individual" in {
              enrolmentCombinationsForIndividuals.foreach { enrolments =>
                withClue(s"For enrolments $enrolments: ") {
                  inSequence {
                    mockAuthWithRetrievals(
                      ConfidenceLevel.L50,
                      Some(AffinityGroup.Organisation),
                      None,
                      None,
                      None,
                      Enrolments(enrolments),
                      Some(retrievedGGCredential(ggCredId))
                    )
                    mockGetSession(Right(None))
                    mockSendAuditEvent(
                      ApplicantServiceStartEndPointAccessed(
                        AuthenticationStatus.Authenticated,
                        Some(expectedIvUrl.stripSuffix(s"?$queryString")),
                        Some(
                          AuthenticationDetails(
                            ggProviderType,
                            ggCredId.value,
                            Some(AffinityGroup.Organisation),
                            Some(EntityType.Individual),
                            ConfidenceLevel.L50
                          )
                        )
                      )
                    )
                  }

                  checkIsRedirect(performAction(), expectedIvUrl)
                }
              }

            }

            "it is not clear from the enrolments and the CL that the user is an individual but " +
              "the user has confirmed they are an individual" in {
                inSequence {
                  mockAuthWithRetrievals(
                    ConfidenceLevel.L200,
                    Some(AffinityGroup.Organisation),
                    None,
                    None,
                    None,
                    Enrolments(Set.empty),
                    Some(retrievedGGCredential(ggCredId))
                  )
                  mockGetSession(Right(None))
                  mockGetUncertainEntityTypeJourney(
                    Right(Some(UncertainEntityTypeJourney(ggCredId, Some(EntityType.Individual))))
                  )
                  mockSendAuditEvent(
                    ApplicantServiceStartEndPointAccessed(
                      AuthenticationStatus.Authenticated,
                      Some(expectedIvUrl.stripSuffix(s"?$queryString")),
                      Some(
                        AuthenticationDetails(
                          ggProviderType,
                          ggCredId.value,
                          Some(AffinityGroup.Organisation),
                          Some(EntityType.Individual),
                          ConfidenceLevel.L200
                        )
                      )
                    )
                  )
                }

                checkIsRedirect(performAction(), expectedIvUrl)
              }

          }

        }

      }

      "redirect to the login page when the user is not logged in" in {
        List[NoActiveSession](
          BearerTokenExpired(),
          MissingBearerToken(),
          InvalidBearerToken(),
          SessionRecordNotFound()
        ).foreach { e =>
          withClue(s"For AuthorisationException $e: ") {
            inSequence {
              mockAuth(EmptyPredicate, retrievals)(Future.failed(e))
              mockSendAuditEvent(
                ApplicantServiceStartEndPointAccessed(AuthenticationStatus.NotAuthenticated, Some(signInUrl), None)
              )
            }

            val result = performAction()
            checkIsRedirect(
              result,
              s"$signInUrl?continue=${s"$selfBaseUrl/tax-check-for-licence/start".urlEncode}&origin=$ggOrigin"
            )
          }
        }
      }

      "redirect to the 'Agents not supported' page" when {

        "the retrieved affinity group is Agent" in {
          inSequence {
            mockAuthWithRetrievals(
              ConfidenceLevel.L50,
              Some(AffinityGroup.Agent),
              Some(completeIndividualLoginData.nino),
              Some(sautr),
              completeIndividualLoginData.emailAddress,
              Enrolments(Set.empty),
              Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
            )
            mockGetSession(Right(None))
            mockSendAuditEvent(
              ApplicantServiceStartEndPointAccessed(
                AuthenticationStatus.Authenticated,
                Some(routes.AgentsController.agentsNotSupported.url),
                Some(
                  AuthenticationDetails(
                    ggProviderType,
                    ggCredId.value,
                    Some(AffinityGroup.Agent),
                    None,
                    ConfidenceLevel.L50
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.AgentsController.agentsNotSupported)
        }

      }

      "redirect to the 'individual details not found' page" when {

        "the call to citizen details doesn't return any data" in {
          inSequence {
            mockAuthWithRetrievals(
              ConfidenceLevel.L250,
              Some(AffinityGroup.Individual),
              Some(completeIndividualLoginData.nino),
              None,
              completeIndividualLoginData.emailAddress,
              Enrolments(Set.empty),
              Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
            )
            mockGetSession(Right(None))
            mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(None))
            mockSendAuditEvent(
              ApplicantServiceStartEndPointAccessed(
                AuthenticationStatus.Authenticated,
                Some(routes.ConfirmIndividualDetailsController.cannotFindDetails.url),
                Some(
                  AuthenticationDetails(
                    ggProviderType,
                    ggCredId.value,
                    Some(AffinityGroup.Individual),
                    Some(EntityType.Individual),
                    ConfidenceLevel.L250
                  )
                )
              )
            )
          }

          checkIsRedirect(performAction(), routes.ConfirmIndividualDetailsController.cannotFindDetails)
        }

      }

    }

  }

  def urlEncode(s: String) = URLEncoder.encode(s, "UTF-8")
}

object StartControllerSpec {

  implicit class RetrievalOps[A, B](val r: ~[A, B]) {
    def and[C](c: C): ~[~[A, B], C] = new ~(r, c)
  }

}
