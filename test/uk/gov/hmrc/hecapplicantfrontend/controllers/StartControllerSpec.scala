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

import cats.data.EitherT
import cats.instances.future._
import com.typesafe.config.ConfigFactory
import play.api.Configuration
import play.api.inject.bind
import play.api.inject.guice.GuiceableModule
import play.api.mvc.Result
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.authorise.EmptyPredicate
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.{AffinityGroup, AuthConnector, AuthorisationException, BearerTokenExpired, ConfidenceLevel, Enrolment, EnrolmentIdentifier, Enrolments, IncorrectCredentialStrength, InsufficientEnrolments, InternalError, InvalidBearerToken, MissingBearerToken, NoActiveSession, SessionRecordNotFound, UnsupportedAffinityGroup, UnsupportedAuthProvider, UnsupportedCredentialRole}
import uk.gov.hmrc.hecapplicantfrontend.config.EnrolmentConfig
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{CitizenDetailsService, JourneyService}
import uk.gov.hmrc.hecapplicantfrontend.models.{CitizenDetails, DateOfBirth, EmailAddress, Error, HECSession, Name}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.http.HeaderCarrier

import java.net.URLEncoder
import java.time.LocalDate
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with JourneyServiceSupport {

  import StartControllerSpec._

  val ivOrigin      = "ivOrigin"
  val ivUrl         = "https://iv:123"
  val selfBaseUrl   = "http://self:456"
  val basGatewayUrl = "https://bas:456"
  val ggOrigin      = "ggOrigin"

  override lazy val additionalConfig: Configuration = Configuration(
    ConfigFactory.parseString(
      s"""
         |iv {
         |  origin = "$ivOrigin"
         |  url = "$ivUrl"
         |  use-relative-urls = false
         |}
         |
         |self.url = "$selfBaseUrl"
         |
         |auth {
         |   bas-gateway.url = "$basGatewayUrl"
         |   gg.origin = "$ggOrigin"
         |}
         |""".stripMargin
    )
  )

  val mockCitizenDetailsService = mock[CitizenDetailsService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[CitizenDetailsService].toInstance(mockCitizenDetailsService),
      bind[JourneyService].toInstance(mockJourneyService)
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

  def mockGetCitizenDetails(nino: NINO)(result: Either[Error, CitizenDetails]) =
    (mockCitizenDetailsService
      .getCitizenDetails(_: NINO)(_: HeaderCarrier))
      .expects(nino, *)
      .returning(EitherT.fromEither(result))

  def retrievedGGCredential(ggCredId: GGCredId) =
    Credentials(ggCredId.value, "GovernmentGateway")

  def retrievedCtEnrolment(ctutr: CTUTR) = Enrolment(
    EnrolmentConfig.CTEnrolment.key,
    List(EnrolmentIdentifier(EnrolmentConfig.CTEnrolment.ctutrIdentifier, ctutr.value)),
    "state",
    None
  )

  val controller = instanceOf[StartController]

  "StartController" when {

    "handling requests to the start endpoint" must {

      val ggCredId = GGCredId("credId")

      val sautr = SAUTR("sautr")

      val emailAddress = EmailAddress("email")

      val ctutr = CTUTR("ctutr")

      val completeIndividualRetrievedData = IndividualRetrievedData(
        ggCredId,
        NINO("nino"),
        Some(sautr),
        Name("First", "Last"),
        DateOfBirth(LocalDate.now()),
        Some(emailAddress)
      )

      val completeCompanyRetrievedData = CompanyRetrievedData(
        ggCredId,
        Some(ctutr),
        Some(emailAddress)
      )

      def performAction(): Future[Result] = controller.start(FakeRequest())

      "proceed" when {

        "existing session data is found" in {
          val session = HECSession(completeIndividualRetrievedData)
          inSequence {
            mockAuthWithRetrievals(ConfidenceLevel.L50, None, None, None, None, Enrolments(Set.empty), None)
            mockGetSession(session)
            mockFirstPge(session)(mockNextCall)
          }

          checkIsRedirect(performAction(), mockNextCall)
        }

        "no session data is found and" when {

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Individaul' and CL250" in {
              List(
                completeIndividualRetrievedData,
                completeIndividualRetrievedData.copy(emailAddress = None),
                completeIndividualRetrievedData.copy(sautr = None)
              ).foreach { individualRetrievedData =>
                val citizenDetails = CitizenDetails(
                  individualRetrievedData.name,
                  individualRetrievedData.dateOfBirth,
                  individualRetrievedData.sautr
                )

                val session = HECSession(individualRetrievedData)

                inSequence {
                  mockAuthWithRetrievals(
                    ConfidenceLevel.L250,
                    Some(AffinityGroup.Individual),
                    Some(individualRetrievedData.nino),
                    None,
                    individualRetrievedData.emailAddress,
                    Enrolments(Set.empty),
                    Some(retrievedGGCredential(individualRetrievedData.ggCredId))
                  )
                  mockGetSession(Right(None))
                  mockGetCitizenDetails(individualRetrievedData.nino)(Right(citizenDetails))
                  mockStoreSession(session)(Right(()))
                  mockFirstPge(session)(mockNextCall)
                }

                checkIsRedirect(performAction(), mockNextCall)
              }
            }

          "no SAUTR is returned in citizen details but one is retrieved from the GG cred" in {
            val citizenDetails = CitizenDetails(
              completeIndividualRetrievedData.name,
              completeIndividualRetrievedData.dateOfBirth,
              None
            )

            val session = HECSession(completeIndividualRetrievedData)

            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualRetrievedData.nino)(Right(citizenDetails))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
            }

            checkIsRedirect(performAction(), mockNextCall)
          }

          "an SAUTR is retrieved in the GG cred and in citizen details" in {
            val citizenDetailsSautr = SAUTR("citizen-details-sautr")
            val ggSautr             = SAUTR("gg-sautr")

            val citizenDetails = CitizenDetails(
              completeIndividualRetrievedData.name,
              completeIndividualRetrievedData.dateOfBirth,
              Some(citizenDetailsSautr)
            )
            val session        = HECSession(completeIndividualRetrievedData.copy(sautr = Some(citizenDetailsSautr)))

            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualRetrievedData.nino),
                Some(ggSautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualRetrievedData.nino)(Right(citizenDetails))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
            }

            checkIsRedirect(performAction(), mockNextCall)
          }

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Organisation' where the only enrolment is an IR-SA one and the user has CL250" in {
              val citizenDetails = CitizenDetails(
                completeIndividualRetrievedData.name,
                completeIndividualRetrievedData.dateOfBirth,
                completeIndividualRetrievedData.sautr
              )

              val session = HECSession(completeIndividualRetrievedData)

              inSequence {
                mockAuthWithRetrievals(
                  ConfidenceLevel.L250,
                  Some(AffinityGroup.Organisation),
                  Some(completeIndividualRetrievedData.nino),
                  None,
                  completeIndividualRetrievedData.emailAddress,
                  Enrolments(Set(Enrolment(EnrolmentConfig.SAEnrolment.key))),
                  Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
                )
                mockGetSession(Right(None))
                mockGetCitizenDetails(completeIndividualRetrievedData.nino)(Right(citizenDetails))
                mockStoreSession(session)(Right(()))
                mockFirstPge(session)(mockNextCall)
              }

              checkIsRedirect(performAction(), mockNextCall)
            }

          "all the necessary data is retrieved for a company" in {
            List(
              completeCompanyRetrievedData,
              completeCompanyRetrievedData.copy(emailAddress = None)
            ).foreach { companyRetrievedData =>
              val session = HECSession(companyRetrievedData)
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
                mockStoreSession(session)(Right(()))
                mockFirstPge(session)(mockNextCall)
              }

              checkIsRedirect(performAction(), mockNextCall)
            }
          }

          "no CTUTR can be found for a company" in {
            val companyData = completeCompanyRetrievedData.copy(ctutr = None)
            val session     = HECSession(companyData)

            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Organisation),
                None,
                None,
                completeCompanyRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeCompanyRetrievedData.ggCredId))
              )
              mockGetSession(Right(None))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)

            }

            checkIsRedirect(performAction(), mockNextCall)
          }

        }
      }

      "return an InternalServerError" when {

        def testIsError(mockActions: () => Unit) = {
          mockActions()

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
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
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
              )
              mockGetSession(Left(Error("")))
            }
          )
        }

        "the retrieved affinity group is Agent" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Agent),
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
              )
              mockGetSession(Right(None))
            }
          )
        }

        "the retrieved affinity group is empty" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                None,
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
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
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(Credentials("id", "OtherProvider"))
              )
              mockGetSession(Right(None))
            }
          )
        }

        "the retrieved credential is empty" in {
          testIsError(() =>
            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L50,
                Some(AffinityGroup.Individual),
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
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
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
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
                Some(completeIndividualRetrievedData.nino),
                Some(sautr),
                completeIndividualRetrievedData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualRetrievedData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualRetrievedData.nino)(Left(Error("")))
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
                completeCompanyRetrievedData.emailAddress,
                Enrolments(Set(retrievedCtEnrolment(ctutr))),
                Some(retrievedGGCredential(completeCompanyRetrievedData.ggCredId))
              )
              mockGetSession(Right(None))
              mockStoreSession(HECSession(completeCompanyRetrievedData))(Left(Error("")))
            }
          )
        }

      }

      "redirect to IV uplift" when {

        def testIsRedirectToIVUplift(mockActions: () => Unit): Unit = {
          val queryString =
            s"origin=$ivOrigin&confidenceLevel=250&" +
              s"completionURL=${urlEncode(s"$selfBaseUrl/tax-check-for-licence/start")}&" +
              s"failureURL=${urlEncode(s"$selfBaseUrl/tax-check-for-licence/failed-iv/callback")}"

          mockActions()
          checkIsRedirect(
            performAction(),
            s"$ivUrl/mdtp/uplift?$queryString"
          )
        }

        "the user has CL50 and" when {

          "the affinity group is 'Individual'" in {
            testIsRedirectToIVUplift(() =>
              inSequence {
                mockAuthWithRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Individual),
                  None,
                  None,
                  None,
                  Enrolments(Set.empty),
                  Some(retrievedGGCredential(ggCredId))
                )
                mockGetSession(Right(None))
              }
            )

          }

          "the affinity group is 'Organisation' and the only enrolment is an IR-SA one" in {
            testIsRedirectToIVUplift(() =>
              inSequence {
                mockAuthWithRetrievals(
                  ConfidenceLevel.L50,
                  Some(AffinityGroup.Organisation),
                  None,
                  None,
                  None,
                  Enrolments(Set(Enrolment(EnrolmentConfig.SAEnrolment.key, Seq.empty, "state"))),
                  Some(retrievedGGCredential(ggCredId))
                )
                mockGetSession(Right(None))
              }
            )
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
          withClue(s"For AuhtorisationException $e: ") {
            mockAuth(EmptyPredicate, retrievals)(Future.failed(e))

            val result = performAction()
            checkIsRedirect(
              result,
              s"$basGatewayUrl?continue=$selfBaseUrl/tax-check-for-licence/start&origin=$ggOrigin"
            )
          }
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
