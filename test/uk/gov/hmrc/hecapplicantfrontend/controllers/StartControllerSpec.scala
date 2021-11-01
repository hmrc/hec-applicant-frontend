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
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals
import uk.gov.hmrc.auth.core.retrieve.{Credentials, ~}
import uk.gov.hmrc.auth.core._
import uk.gov.hmrc.hecapplicantfrontend.config.EnrolmentConfig
import uk.gov.hmrc.hecapplicantfrontend.models.HECSession.{CompanyHECSession, IndividualHECSession}
import uk.gov.hmrc.hecapplicantfrontend.models.LoginData.{CompanyLoginData, IndividualLoginData}
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, GGCredId, NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{CitizenDetails, DateOfBirth, EmailAddress, Error, Name, TaxCheckListItem}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.{CitizenDetailsService, JourneyService, TaxCheckService}
import uk.gov.hmrc.hecapplicantfrontend.util.StringUtils.StringOps
import uk.gov.hmrc.http.HeaderCarrier

import java.net.URLEncoder
import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StartControllerSpec extends ControllerSpec with AuthSupport with SessionSupport with JourneyServiceSupport {

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

  val mockCitizenDetailsService = mock[CitizenDetailsService]
  val mockTaxCheckService       = mock[TaxCheckService]

  override val overrideBindings =
    List[GuiceableModule](
      bind[AuthConnector].toInstance(mockAuthConnector),
      bind[SessionStore].toInstance(mockSessionStore),
      bind[CitizenDetailsService].toInstance(mockCitizenDetailsService),
      bind[TaxCheckService].toInstance(mockTaxCheckService),
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

  def mockGetUnexpiredTaxCheckCodes(result: Either[Error, List[TaxCheckListItem]]) =
    (mockTaxCheckService
      .getUnexpiredTaxCheckCodes()(_: HeaderCarrier))
      .expects(*)
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

      val sautr = SAUTR("1234567895")

      val emailAddress = EmailAddress("email")

      val ctutr = CTUTR("1234567895")

      val nino = NINO("AB123456C")

      val completeIndividualLoginData =
        IndividualLoginData(
          ggCredId,
          nino,
          Some(sautr),
          Name("First", "Last"),
          DateOfBirth(LocalDate.now()),
          Some(emailAddress)
        )

      val completeCompanyLoginData = CompanyLoginData(
        ggCredId,
        Some(ctutr),
        Some(emailAddress)
      )

      def performAction(): Future[Result] = controller.start(FakeRequest())

      "proceed" when {

        "existing session data is found" in {
          val session = IndividualHECSession.newSession(completeIndividualLoginData)
          inSequence {
            mockAuthWithRetrievals(ConfidenceLevel.L50, None, None, None, None, Enrolments(Set.empty), None)
            mockGetSession(session)
            mockGetUnexpiredTaxCheckCodes(Right(List.empty))
            mockStoreSession(session)(Right(()))
            mockFirstPge(session)(mockNextCall)
          }

          checkIsRedirect(performAction(), mockNextCall)
        }

        "no session data is found and" when {

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Individaul' and CL250" in {
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

                val session = IndividualHECSession.newSession(individualRetrievedData)
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
                  mockGetUnexpiredTaxCheckCodes(Right(List.empty))
                  mockStoreSession(session)(Right(()))
                  mockFirstPge(session)(mockNextCall)
                }

                checkIsRedirect(performAction(), mockNextCall)
              }
            }

          "no SAUTR is returned in citizen details but one is retrieved from the GG cred" in {
            val citizenDetails = CitizenDetails(
              completeIndividualLoginData.name,
              completeIndividualLoginData.dateOfBirth,
              None
            )

            val session = IndividualHECSession.newSession(completeIndividualLoginData)

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
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails))
              mockGetUnexpiredTaxCheckCodes(Right(List.empty))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
            }

            checkIsRedirect(performAction(), mockNextCall)
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
              IndividualHECSession.newSession(completeIndividualLoginData.copy(sautr = Some(citizenDetailsSautr)))

            inSequence {
              mockAuthWithRetrievals(
                ConfidenceLevel.L250,
                Some(AffinityGroup.Individual),
                Some(completeIndividualLoginData.nino),
                Some(ggSautr),
                completeIndividualLoginData.emailAddress,
                Enrolments(Set.empty),
                Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
              )
              mockGetSession(Right(None))
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails))
              mockGetUnexpiredTaxCheckCodes(Right(List.empty))
              mockStoreSession(session)(Right(()))
              mockFirstPge(session)(mockNextCall)
            }

            checkIsRedirect(performAction(), mockNextCall)
          }

          "all the necessary data is retrieved for an individual with affinity group " +
            "'Organisation' where the only enrolment other than the NINO enrolment is an IR-SA one " +
            "and the user has CL250" in {
              val citizenDetails = CitizenDetails(
                completeIndividualLoginData.name,
                completeIndividualLoginData.dateOfBirth,
                completeIndividualLoginData.sautr
              )

              val session = IndividualHECSession.newSession(completeIndividualLoginData)

              inSequence {
                mockAuthWithRetrievals(
                  ConfidenceLevel.L250,
                  Some(AffinityGroup.Organisation),
                  Some(completeIndividualLoginData.nino),
                  None,
                  completeIndividualLoginData.emailAddress,
                  Enrolments(
                    Set(
                      Enrolment(EnrolmentConfig.SAEnrolment.key),
                      Enrolment(EnrolmentConfig.NINOEnrolment.key)
                    )
                  ),
                  Some(retrievedGGCredential(completeIndividualLoginData.ggCredId))
                )
                mockGetSession(Right(None))
                mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails))
                mockGetUnexpiredTaxCheckCodes(Right(List.empty))
                mockStoreSession(session)(Right(()))
                mockFirstPge(session)(mockNextCall)
              }

              checkIsRedirect(performAction(), mockNextCall)
            }

          "all the necessary data is retrieved for a company" in {
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

        "the retrieved affinity group is Agent" in {
          testIsError(() =>
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
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails))
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
              mockGetCitizenDetails(completeIndividualLoginData.nino)(Right(citizenDetails))
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
            s"$ivUrl$ivLocation/uplift?$queryString"
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
              s"$signInUrl?continue=${(s"$selfBaseUrl/tax-check-for-licence/start").urlEncode}&origin=$ggOrigin"
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
