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

import cats.data.EitherT
import cats.instances.future._
import org.scalamock.scalatest.MockFactory
import play.api.mvc.Request
import uk.gov.hmrc.hecapplicantfrontend.models.{Error, HECSession}
import uk.gov.hmrc.hecapplicantfrontend.repos.SessionStore

import scala.concurrent.ExecutionContext

trait SessionSupport { this: MockFactory =>

  val mockSessionStore: SessionStore = mock[SessionStore]

  def mockGetSession(result: Either[Error, Option[HECSession]])(implicit ec: ExecutionContext) =
    (mockSessionStore
      .get()(_: Request[_]))
      .expects(*)
      .returning(EitherT.fromEither(result))

  def mockGetSession(session: HECSession)(implicit ec: ExecutionContext) =
    (mockSessionStore
      .get()(_: Request[_]))
      .expects(*)
      .returning(EitherT.pure(Some(session)))

  def mockStoreSession(session: HECSession)(result: Either[Error, Unit])(implicit ec: ExecutionContext) =
    (mockSessionStore
      .store(_: HECSession)(_: Request[_]))
      .expects(session, *)
      .returning(EitherT.fromEither(result))

}
