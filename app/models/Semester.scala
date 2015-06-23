package models

import java.util.UUID
import controllers.crud.JsonSerialisation
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class Semester(name: String, startDate: String, endDate: String, examPeriod: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object Semester extends UriGenerator[Semester] with JsonSerialisation[Semester] {
  def generateUri(semester: Semester)(implicit ns: Namespace): String = s"${ns}semesters/${semester.id}"

  override implicit def reads: Reads[Semester] = Json.reads[Semester]

  override implicit def writes: Writes[Semester] = Json.writes[Semester]
}