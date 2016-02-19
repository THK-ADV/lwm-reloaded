package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import org.joda.time.LocalDate
import play.api.libs.json.{Json, Reads, Writes}

case class Semester(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate, id: UUID) extends UniqueEntity

case class SemesterProtocol(label: String, abbreviation: String, start: LocalDate, end: LocalDate, examStart: LocalDate)

object Semester extends UriGenerator[Semester] with JsonSerialisation[SemesterProtocol, Semester] {

  override implicit def reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]

  override implicit def writes: Writes[Semester] = Json.writes[Semester]

  override def base: String = "semesters"
}