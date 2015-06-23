package models.schedules

import java.util.UUID
import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class StudentSchedule(id: UUID = UUID.randomUUID()) extends UniqueEntity

// not included associations: []

object StudentSchedule extends UriGenerator[StudentSchedule] with JsonSerialisation[StudentSchedule] {
  def generateUri(studentSchedule: StudentSchedule)(implicit ns: Namespace): String = s"${ns}studentSchedules/${studentSchedule.id}"

  override implicit def reads: Reads[StudentSchedule] = Json.reads[StudentSchedule]

  override implicit def writes: Writes[StudentSchedule] = Json.writes[StudentSchedule]
}