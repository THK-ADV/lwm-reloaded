package models.labwork

import java.util.UUID

import controllers.UserController
import controllers.crud.JsonSerialisation
import models._
import models.users.{Employee, User}
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.json._

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

case class ScheduleEntryAtom(labwork: LabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: Room, supervisor: Set[User], group: Group, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[Schedule, Schedule, ScheduleAtom] {

  //import ScheduleEntry.format

  lazy val empty = Schedule(UUID.randomUUID, Set.empty[ScheduleEntry])

  override def base: String = "schedules"

  override implicit def reads: Reads[Schedule] = Json.reads[Schedule]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]

  override implicit def writesAtom: Writes[ScheduleAtom] = new Writes[ScheduleAtom] {
    override def writes(o: ScheduleAtom): JsValue = {
      val json = Json.obj(
        "labwork" -> Labwork.writes.writes(o.labwork),
        "entries" -> JsArray(o.entries map ScheduleEntry.writesAtom.writes toSeq)
      )

      o.invalidated.fold(json)(d => json + ("invalidated" -> JsString(d.toString()))) + ("id" -> JsString(o.id.toString))
    }
  }

  implicit def setAtomicWrites: Writes[Set[ScheduleAtom]] = Writes.set[ScheduleAtom]
}

object ScheduleEntry extends UriGenerator[ScheduleEntry] with JsonSerialisation[ScheduleEntry, ScheduleEntry, ScheduleEntryAtom] {

  import models.labwork.Labwork.formatAtom

  override implicit def reads: Reads[ScheduleEntry] = Json.reads[ScheduleEntry]

  override implicit def writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]

  override implicit def writesAtom: Writes[ScheduleEntryAtom] = new Writes[ScheduleEntryAtom] {
    override def writes(o: ScheduleEntryAtom): JsValue = {
      val json = Json.obj(
        "labwork" -> Labwork.writesAtom.writes(o.labwork),
        "start" -> o.start,
        "end" -> o.end,
        "date" -> o.date,
        "room" -> Room.writes.writes(o.room),
        "supervisor" -> JsArray(o.supervisor map UserController.writes.writes toSeq),
        "group" -> Group.writes.writes(o.group)
      )

      o.invalidated.fold(json)(d => json + ("invalidated" -> JsString(d.toString()))) + ("id" -> JsString(o.id.toString))
    }
  }

  //implicit def format: Format[ScheduleEntryAtom] = Json.format[ScheduleEntryAtom]

  implicit def setAtomicWrites: Writes[Set[ScheduleEntryAtom]] = Writes.set[ScheduleEntryAtom]

  override def base: String = "scheduleEntry"
}