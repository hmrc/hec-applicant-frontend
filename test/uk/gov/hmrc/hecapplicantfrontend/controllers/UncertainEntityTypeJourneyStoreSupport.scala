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
import cats.instances.future._
import org.scalamock.handlers.CallHandler1
import org.scalamock.scalatest.MockFactory
import play.api.mvc.Request
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, UncertainEntityTypeJourney}
import uk.gov.hmrc.hecapplicantfrontend.repos.UncertainEntityTypeJourneyStore

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

trait UncertainEntityTypeJourneyStoreSupport { this: MockFactory =>

  val mockUncertainEntityTypeJourneyStore = mock[UncertainEntityTypeJourneyStore]

  def mockGetUncertainEntityTypeJourney(
    result: Either[Error, Option[UncertainEntityTypeJourney]]
  ): CallHandler1[Request[_], EitherT[Future, Error, Option[UncertainEntityTypeJourney]]] =
    (mockUncertainEntityTypeJourneyStore
      .get()(_: Request[_]))
      .expects(*)
      .returning(EitherT.fromEither(result))

  def mockGetUncertainEntityTypeJourney(
    journey: UncertainEntityTypeJourney
  ): CallHandler1[Request[_], EitherT[Future, Error, Option[UncertainEntityTypeJourney]]] =
    mockGetUncertainEntityTypeJourney(Right(Some(journey)))

  def mockUpdateUncertainEntityTypeJourney(journey: UncertainEntityTypeJourney)(
    result: Either[Error, Unit]
  ) =
    (mockUncertainEntityTypeJourneyStore
      .store(_: UncertainEntityTypeJourney)(_: Request[_]))
      .expects(journey, *)
      .returning(EitherT.fromEither(result))

}
