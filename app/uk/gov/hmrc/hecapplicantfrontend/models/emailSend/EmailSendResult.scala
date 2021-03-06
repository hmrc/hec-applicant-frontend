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

package uk.gov.hmrc.hecapplicantfrontend.models.emailSend

import ai.x.play.json.Jsonx
import cats.Eq
import play.api.libs.json.Format
import ai.x.play.json.SingletonEncoder.simpleName
import ai.x.play.json.implicits.formatSingleton

sealed trait EmailSendResult extends Product with Serializable

object EmailSendResult {

  case object EmailSent extends EmailSendResult
  case object EmailSentFailure extends EmailSendResult

  implicit val eq: Eq[EmailSendResult] = Eq.fromUniversalEquals

  @SuppressWarnings(Array("org.wartremover.warts.All"))
  implicit val format: Format[EmailSendResult] = Jsonx.formatSealed[EmailSendResult]
}
