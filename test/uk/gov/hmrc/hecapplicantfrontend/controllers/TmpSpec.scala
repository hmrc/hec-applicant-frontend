package uk.gov.hmrc.hecapplicantfrontend.controllers

import uk.gov.hmrc.hecapplicantfrontend.models.TaxYear
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{CTUTR, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.services.TaxCheckService
import uk.gov.hmrc.http.HeaderCarrier

import java.time.LocalDate
import scala.concurrent.Await
import scala.concurrent.duration._

class TmpSpec extends ControllerSpec {

  "Test" in {
    implicit val hc = HeaderCarrier()

    val taxService = instanceOf[TaxCheckService]
    val saResult   = taxService.getSAStatus(SAUTR("1234568795"), TaxYear(2022))
    val ctResult   = taxService.getCTStatus(CTUTR("1234568795"), LocalDate.now().minusYears(1L), LocalDate.now())
    println(s"SA status came back with ${Await.result(saResult.value, 10.seconds)}\n\n")
    println(s"CT status came back with ${Await.result(ctResult.value, 10.seconds)}\n\n")
  }

}
