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
import play.api.mvc.Call
import uk.gov.hmrc.hecapplicantfrontend.controllers.actions.RequestWithSessionData
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.services.JourneyService
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.ExecutionContext.Implicits.global

trait JourneyServiceSupport { this: ControllerSpec =>

  val mockJourneyService: JourneyService = mock[JourneyService]

  val mockNextCall: Call = Call("", "/next")

  val mockPreviousCall: Call = Call("", "/previous")

  def mockJourneyServiceUpdateAndNext(currentPage: Call, currentSession: HECSession, updatedSession: HECSession)(
    result: Either[Error, Call]
  ) =
    (mockJourneyService
      .updateAndNext(_: Call, _: HECSession)(_: RequestWithSessionData[_], _: HeaderCarrier))
      .expects(where { case (c, s, r, _) =>
        assert(c === currentPage)
        assert(s === updatedSession)
        assert(r.sessionData === currentSession)
        true
      })
      .returning(EitherT.fromEither(result))

  def mockJourneyServiceGetPrevious(currentPage: Call, currentSession: HECSession)(result: Call) =
    (mockJourneyService
      .previous(_: Call)(_: RequestWithSessionData[_]))
      .expects(where { case (c, r) =>
        assert(c === currentPage)
        assert(r.sessionData === currentSession)
        true
      })
      .returning(result)

  def mockFirstPge(session: HECSession)(result: Call) =
    (mockJourneyService
      .firstPage(_: HECSession))
      .expects(session)
      .returning(result)

}
