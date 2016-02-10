package models.semester

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import play.api.libs.json.{Json, Reads, Writes}

case class Semester(name: String, startDate: String, endDate: String, examPeriod: String, blacklist: Blacklist, id: UUID) extends UniqueEntity

case class SemesterProtocol(name: String, startDate: String, endDate: String, examPeriod: String, blacklist: Blacklist = Blacklist.empty)

object Semester extends UriGenerator[Semester] with JsonSerialisation[SemesterProtocol, Semester] {

  import Blacklist._

  override implicit def reads: Reads[SemesterProtocol] = Json.reads[SemesterProtocol]

  override implicit def writes: Writes[Semester] = Json.writes[Semester]

  override def base: String = "semesters"
}