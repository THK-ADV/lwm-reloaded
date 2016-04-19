package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import models.users.Employee
import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.json.{Format, Json, Reads, Writes}

case class Schedule(labwork: UUID, entries: Set[ScheduleEntry], published: Boolean = false, id: UUID = Schedule.randomUUID) extends UniqueEntity

case class ScheduleEntry(labwork: UUID,
                         start: LocalTime,
                         end: LocalTime,
                         date: LocalDate,
                         room: UUID,
                         supervisor: UUID,
                         group: UUID,
                         id: UUID = ScheduleEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleEntry(l, s, e, d, r, su, g, i) =>
        l == labwork &&
        s.isEqual(start) &&
        e.isEqual(end) &&
        d.isEqual(date) &&
        r == room &&
        su == supervisor &&
        g == group &&
        i == id
    case _ => false
  }
}

/**
  * Atoms
  */

case class ScheduleAtom(labwork: Labwork, entries: Set[ScheduleEntryAtom], published: Boolean, id: UUID)

case class ScheduleEntryAtom(labwork: Labwork, start: LocalTime, end: LocalTime, date: LocalDate, room: Room, supervisor: Employee, group: Group, id: UUID)

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[Schedule, Schedule] {

  import ScheduleEntry.format

  lazy val empty = Schedule(UUID.randomUUID, Set.empty[ScheduleEntry])

  override def base: String = "schedules"

  override implicit def reads: Reads[Schedule] = Json.reads[Schedule]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]

  implicit def atomicWrites: Writes[ScheduleAtom] = Json.writes[ScheduleAtom]

  implicit def setAtomicWrites: Writes[Set[ScheduleAtom]] = Writes.set[ScheduleAtom](atomicWrites)
}

object ScheduleEntry extends UriGenerator[ScheduleEntry] with JsonSerialisation[ScheduleEntry, ScheduleEntry] {

  override implicit def reads: Reads[ScheduleEntry] = Json.reads[ScheduleEntry]

  override implicit def writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]

  implicit def atomicWrites: Writes[ScheduleEntryAtom] = Json.writes[ScheduleEntryAtom]

  implicit def format: Format[ScheduleEntryAtom] = Json.format[ScheduleEntryAtom]

  implicit def setAtomicWrites: Writes[Set[ScheduleEntryAtom]] = Writes.set[ScheduleEntryAtom](atomicWrites)

  override def base: String = "scheduleEntry"
}