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

package uk.gov.hmrc.hecapplicantfrontend.models

import play.api.libs.json.*

sealed trait AuthenticationStatus extends Product with Serializable

object AuthenticationStatus {

  case object Authenticated extends AuthenticationStatus

  case object NotAuthenticated extends AuthenticationStatus

  implicit val format: Format[AuthenticationStatus] = new Format[AuthenticationStatus] {
    override def reads(json: JsValue): JsResult[AuthenticationStatus] = json match {
      case JsString("Authenticated")    => JsSuccess(Authenticated)
      case JsString("NotAuthenticated") => JsSuccess(NotAuthenticated)
      case _                            => JsError(s"Unknown authentication status: ${json.toString()}")
    }

    override def writes(o: AuthenticationStatus): JsValue = JsString(o.toString)
  }

}
