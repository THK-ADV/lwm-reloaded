package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models._
import models.users.Employee
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.json._
import play.api.libs.functional.syntax._
import utils.Ops.JsPathX

case class Schedule(labwork: UUID, entries: Set[ScheduleEntry], invalidated: Option[DateTime] = None, id: UUID = Schedule.randomUUID) extends UniqueEntity

case class ScheduleEntry(labwork: UUID,
                         start: LocalTime,
                         end: LocalTime,
                         date: LocalDate,
                         room: UUID,
                         supervisor: Set[UUID],
                         group: UUID,
                         invalidated: Option[DateTime] = None,
                         id: UUID = ScheduleEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case ScheduleEntry(l, s, e, d, r, su, g, _, i) =>
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

case class ScheduleAtom(labwork: Labwork, entries: Set[ScheduleEntryAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class ScheduleEntryAtom(labwork: LabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: Room, supervisor: Set[Employee], group: Group, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[Schedule, Schedule, ScheduleAtom] {

  //import ScheduleEntry.format

  lazy val empty = Schedule(UUID.randomUUID, Set.empty[ScheduleEntry])

  override def base: String = "schedules"

  override implicit def reads: Reads[Schedule] = Json.reads[Schedule]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]

  override implicit def writesAtom: Writes[ScheduleAtom] = ScheduleAtom.writesAtom

  implicit def setAtomicWrites: Writes[Set[ScheduleAtom]] = Writes.set[ScheduleAtom]
}

object ScheduleAtom{
  implicit def writesAtom: Writes[ScheduleAtom] = (
      (JsPath \ "labwork").write[Labwork] and
      (JsPath \ "entries").writeSet[ScheduleEntryAtom] and
      (JsPath \ "invalidated").write[Option[DateTime]] and
      (JsPath \ "id").write[UUID]
    )(unlift(ScheduleAtom.unapply))
}

object ScheduleEntry extends UriGenerator[ScheduleEntry] with JsonSerialisation[ScheduleEntry, ScheduleEntry, ScheduleEntryAtom] {

  //import models.labwork.Labwork.formatAtom

  override implicit def reads: Reads[ScheduleEntry] = Json.reads[ScheduleEntry]

  override implicit def writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]

  override implicit def writesAtom: Writes[ScheduleEntryAtom] = ScheduleEntryAtom.writesAtom

  //implicit def format: Format[ScheduleEntryAtom] = Json.format[ScheduleEntryAtom]

  implicit def setAtomicWrites: Writes[Set[ScheduleEntryAtom]] = Writes.set[ScheduleEntryAtom]

  override def base: String = "scheduleEntry"
}

object ScheduleEntryAtom{

  implicit def writesAtom: Writes[ScheduleEntryAtom] = (
    (JsPath \ "labwork").write[LabworkAtom] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "supervisor").writeSet[Employee](Employee.writes) and
      (JsPath \ "group").write[Group] and
      (JsPath \ "invalidated").write[Option[DateTime]] and
      (JsPath \ "id").write[UUID]
    )(unlift(ScheduleEntryAtom.unapply))
}
