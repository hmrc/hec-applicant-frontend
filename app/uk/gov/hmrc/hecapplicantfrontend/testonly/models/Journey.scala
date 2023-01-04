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

package uk.gov.hmrc.hecapplicantfrontend.testonly.models

import cats.Eq

sealed trait Journey extends Product with Serializable

object Journey {

  case object IndividualNoSA extends Journey

  case object IndividualNoSANoGGEmail extends Journey

  case object IndividualSAReturnFound extends Journey

  case object IndividualSANoticeToFileIssued extends Journey

  case object IndividualSANoReturnFound extends Journey

  case object IndividualSAReturnFoundExistingTaxCheck extends Journey

  case object CompanyNoCTEnrolment extends Journey

  case object CompanyCTReturnFound extends Journey

  case object CompanyCTNoticeToFileIssued extends Journey

  case object CompanyCTNoReturnFound extends Journey

  case object CompanyCTNoAccountingPeriods extends Journey

  implicit val eq: Eq[Journey] = Eq.fromUniversalEquals

}
