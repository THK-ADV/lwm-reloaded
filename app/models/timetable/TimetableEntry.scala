package models.timetable

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import play.api.libs.json.{Json, Reads, Writes}

case class TimetableEntry(supervisor: String, room: String, startTime: String, endTime: String, id: UUID) extends UniqueEntity

case class TimetableEntryProtocol(supervisor: String, room: String, startTime: String, endTime: String)

object TimetableEntry extends UriGenerator[TimetableEntry] with JsonSerialisation[TimetableEntryProtocol, TimetableEntry] {

  override implicit def reads: Reads[TimetableEntryProtocol] = Json.reads[TimetableEntryProtocol]

  override implicit def writes: Writes[TimetableEntry] = Json.writes[TimetableEntry]

  override def base: String = "timetableEntries"
}
