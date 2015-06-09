package models.timetable

import java.util.UUID

import controllers.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}
import store.Namespace

case class TimetableEntry(supervisor: String, room: String, startTime: String, endTime: String, id: UUID = UUID.randomUUID()) extends UniqueEntity

object TimetableEntry extends UriGenerator[TimetableEntry] with JsonSerialisation[TimetableEntry] {
  def generateUri(timetableEntry: TimetableEntry)(implicit ns: Namespace): String = s"${ns}timetableEntries/${timetableEntry.id}"

  override implicit def reads: Reads[TimetableEntry] = Json.reads[TimetableEntry]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]
}
