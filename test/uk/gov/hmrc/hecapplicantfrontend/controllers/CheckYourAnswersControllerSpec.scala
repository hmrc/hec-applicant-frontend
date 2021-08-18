package uk.gov.hmrc.hecapplicantfrontend.controllers

import play.api.inject.bind
import play.api.test.FakeRequest
import play.api.test.Helpers._
import uk.gov.hmrc.auth.core.AuthConnector
import uk.gov.hmrc.hecapplicantfrontend.models.{DateOfBirth, EntityType, HECSession, Name, TaxSituation, UserAnswers}
import uk.gov.hmrc.hecapplicantfrontend.models.RetrievedApplicantData.{CompanyRetrievedData, IndividualRetrievedData}
import uk.gov.hmrc.hecapplicantfrontend.models.UserAnswers.CompleteUserAnswers
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{GGCredId, NINO}
import uk.gov.hmrc.hecapplicantfrontend.models.licence.{LicenceExpiryDate, LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.hecapplicantfrontend.util.TimeUtils

import java.time.LocalDate
import scala.concurrent.ExecutionContext.Implicits.global

class CheckYourAnswersControllerSpec
    extends ControllerSpec
    with JourneyServiceSupport
    with AuthSupport
    with SessionSupport
    with AuthAndSessionDataBehaviour {

  override def overrideBindings = List(
    bind[AuthConnector].toInstance(mockAuthConnector),
    bind[SessionStore].toInstance(mockSessionStore),
    bind[JourneyService].toInstance(mockJourneyService)
  )

  val controller = instanceOf[CheckYourAnswersController]

  val individualRetrievedData =
    IndividualRetrievedData(GGCredId(""), NINO(""), None, Name("", ""), DateOfBirth(LocalDate.now()), None)
  val companyRetrievedData    = CompanyRetrievedData(GGCredId(""), None, None)

  "CheckYourAnswersController" when {

    "handling requests to display the check your answers page" must {

      def performAction() = controller.checkYourAnswers(FakeRequest())

      behave like (authAndSessionDataBehaviour(performAction))

      "show an error page" when {

        "there are no complete answers in session" in {
          val session = HECSession(individualRetrievedData, UserAnswers.empty, None)

          inSequence {
            mockAuthWithNoRetrievals()
            mockGetSession(session)
          }

          status(performAction()) shouldBe INTERNAL_SERVER_ERROR
        }

      }

      "display the page" in {
        val answers = CompleteUserAnswers(
          LicenceType.ScrapMetalMobileCollector,
          LicenceExpiryDate(TimeUtils.today()),
          LicenceTimeTrading.ZeroToTwoYears,
          LicenceValidityPeriod.UpToTwoYears,
          TaxSituation.PAYE,
          EntityType.Individual
        )

      }

    }

  }

}
