package models.schedule

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.{UniqueEntity, UriGenerator}
import org.joda.time.DateTime
import play.api.libs.json.{Json, Reads, Writes, Format}

case class Schedule(labwork: UUID, entries: Set[ScheduleEntry], id: UUID) extends UniqueEntity

case class ScheduleProtocol(labwork: UUID, entries: Set[ScheduleEntry])

case class ScheduleEntry(start: DateTime, end: DateTime, day: DateTime, date: DateTime, room: UUID, supervisor: UUID, group: UUID, id: UUID) extends UniqueEntity

case class ScheduleEntryProtocol(start: DateTime, end: DateTime, day: DateTime, date: DateTime, room: UUID, supervisor: UUID, group: UUID)

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[ScheduleProtocol, Schedule] {

  override def base: String = "schedules"

  override implicit def reads: Reads[ScheduleProtocol] = Json.reads[ScheduleProtocol]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]
}

object ScheduleEntry extends UriGenerator[ScheduleEntry] with JsonSerialisation[ScheduleEntryProtocol, ScheduleEntry] {

  implicit def format: Format[ScheduleEntry] = Json.format[ScheduleEntry]

  override def base: String = "scheduleEntries"

  override implicit def reads: Reads[ScheduleEntryProtocol] = Json.reads[ScheduleEntryProtocol]

  override implicit def writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]
}