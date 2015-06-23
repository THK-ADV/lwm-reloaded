package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}

case class Semester(name: String, startDate: String, endDate: String, examPeriod: String, id: UUID) extends UniqueEntity

case class SemesterProtocol(name: String, startDate: String, endDate: String, examPeriod: String)

object Semester extends UriGenerator[Semester] with JsonSerialisation[SemesterProtocol, Semester] {

  override implicit def reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]

  override implicit def writes: Writes[Semester] = Json.writes[Semester]

  override def base: String = "semesters"
}