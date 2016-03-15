package models.schedule

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Employee
import models._
import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.json.{Json, Reads, Writes, Format}

case class Schedule(labwork: UUID, entries: Set[ScheduleEntry], published: Boolean = false, id: UUID = Schedule.randomUUID) extends UniqueEntity

case class ScheduleEntry(start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: UUID, group: UUID) {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleEntry(s, e, d, r, su, g) =>
      s.isEqual(start) &&
        e.isEqual(end) &&
        d.isEqual(date) &&
        r == room &&
        su == supervisor &&
        g == group
    case _ => false
  }
}

case class ScheduleProtocol(labwork: UUID, entries: Set[ScheduleEntry], published: Boolean)

case class ScheduleAtom(labwork: Labwork, entries: Set[ScheduleEntryAtom], published: Boolean, id: UUID)

case class ScheduleEntryAtom(start: LocalTime, end: LocalTime, date: LocalDate, room: Room, supervisor: Employee, group: Group)

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[ScheduleProtocol, Schedule] {
  import ScheduleEntry.format

  override def base: String = "schedules"

  override implicit def reads: Reads[ScheduleProtocol] = Json.reads[ScheduleProtocol]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]

  implicit def atomicWrites: Writes[ScheduleAtom] = Json.writes[ScheduleAtom]

  implicit def setAtomicWrites: Writes[Set[ScheduleAtom]] = Writes.set[ScheduleAtom](atomicWrites)
}

object ScheduleEntry extends JsonSerialisation[ScheduleEntry, ScheduleEntry] {

  override implicit def reads: Reads[ScheduleEntry] = Json.reads[ScheduleEntry]

  override implicit def writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]

  implicit def atomicWrites: Writes[ScheduleEntryAtom] = Json.writes[ScheduleEntryAtom]

  implicit def format: Format[ScheduleEntryAtom] = Json.format[ScheduleEntryAtom]

  implicit def setAtomicWrites: Writes[Set[ScheduleEntryAtom]] = Writes.set[ScheduleEntryAtom](atomicWrites)
}