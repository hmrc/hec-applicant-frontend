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

import uk.gov.hmrc.hecapplicantfrontend.models.TaxYear
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.services.TaxCheckService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.Await
import scala.concurrent.duration._

class TmpSpec extends ControllerSpec {

  "Test" ignore {
    implicit val hc: HeaderCarrier = HeaderCarrier()

    val taxService = instanceOf[TaxCheckService]
    val saResult   = taxService.getSAStatus(SAUTR("1234568795"), TaxYear(2022))
    val ctResult   = taxService.getCTStatus(CTUTR("1234568795"), LocalDate.now().minusYears(1L), LocalDate.now())
    println(s"SA status came back with ${Await.result(saResult.value, 10.seconds)}\n\n")
    println(s"CT status came back with ${Await.result(ctResult.value, 10.seconds)}\n\n")
  }

}
