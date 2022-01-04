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

package uk.gov.hmrc.hecapplicantfrontend.services

import cats.data.Validated.Valid
import cats.data.{EitherT, Validated, ValidatedNel}
import cats.instances.future._
import cats.instances.int._
import cats.syntax.apply._
import cats.syntax.either._
import cats.syntax.option._
import cats.syntax.eq._
import play.api.http.Status.OK
import play.api.libs.json.{Json, Reads}
import uk.gov.hmrc.hecapplicantfrontend.connectors.CitizenDetailsConnector
import uk.gov.hmrc.hecapplicantfrontend.models.ids.{NINO, SAUTR}
import uk.gov.hmrc.hecapplicantfrontend.models.{CitizenDetails, DateOfBirth, Error, Name}
import uk.gov.hmrc.hecapplicantfrontend.util.HttpResponseOps._
import uk.gov.hmrc.http.HeaderCarrier

import com.google.inject.{ImplementedBy, Inject, Singleton}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

@ImplementedBy(classOf[CitizenDetailsServiceImpl])
trait CitizenDetailsService {

  def getCitizenDetails(nino: NINO)(implicit hc: HeaderCarrier): EitherT[Future, Error, CitizenDetails]

}

@Singleton
class CitizenDetailsServiceImpl @Inject() (
  citizenDetailsConnector: CitizenDetailsConnector
)(implicit ec: ExecutionContext)
    extends CitizenDetailsService {

  import CitizenDetailServiceImpl._

  override def getCitizenDetails(nino: NINO)(implicit hc: HeaderCarrier): EitherT[Future, Error, CitizenDetails] =
    citizenDetailsConnector.getCitizenDetails(nino).subflatMap { httpResponse =>
      if (httpResponse.status =!= OK)
        Left(Error(s"Response to get citizen details came back with status ${httpResponse.status}"))
      else
        httpResponse
          .parseJSON[CidPerson]
          .leftMap(Error(_))
          .flatMap(toCitizenDetails)

    }

  @SuppressWarnings(Array("org.wartremover.warts.Any"))
  private def toCitizenDetails(cidPerson: CidPerson): Either[Error, CitizenDetails] = {
    val nameValidation: ValidatedNel[String, Name] =
      cidPerson.name.flatMap(_.current) match {
        case Some(CidName(Some(firstName), Some(lastName))) => Valid(Name(firstName, lastName))
        case _                                              => Validated.invalidNel("Could not find valid name")
      }

    val dateOfBirthValidation: ValidatedNel[String, DateOfBirth] =
      cidPerson.dateOfBirth match {
        case None    => Validated.invalidNel("Could not find date of birth")
        case Some(s) =>
          Try(LocalDate.parse(s, dateOfBirthFormatter))
            .fold(
              _ => Validated.invalidNel("Could not parse date of birth"),
              d => Valid(DateOfBirth(d))
            )
      }

    val sautrValidation: ValidatedNel[String, Option[SAUTR]] =
      cidPerson.ids.sautr match {
        case None    => Valid(None)
        case Some(s) => SAUTR.fromString(s).toValidNel("Got invalid SAUTR").map(Some(_))
      }

    (nameValidation, dateOfBirthValidation, sautrValidation)
      .mapN((name, dob, sautr) => CitizenDetails(name, dob, sautr))
      .toEither
      .leftMap(errors => Error(s"Invalid details found: [${errors.toList.mkString(";")}]"))

  }

  private val dateOfBirthFormatter = DateTimeFormatter.ofPattern("ddMMyyyy")

}

object CitizenDetailServiceImpl {

  final case class CidName(firstName: Option[String], lastName: Option[String])

  final case class CidNames(current: Option[CidName])

  final case class TaxIds(sautr: Option[String])

  final case class CidPerson(name: Option[CidNames], ids: TaxIds, dateOfBirth: Option[String])

  implicit val cidNameReads: Reads[CidName]     = Json.reads[CidName]
  implicit val cidNamesReads: Reads[CidNames]   = Json.reads[CidNames]
  implicit val taxIdsReads: Reads[TaxIds]       = Json.reads[TaxIds]
  implicit val cidPersonReads: Reads[CidPerson] = Json.reads[CidPerson]

}
