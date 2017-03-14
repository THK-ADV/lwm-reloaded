package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
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

case class ScheduleAtom(labwork: SesameLabworkAtom, entries: Set[ScheduleEntryAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class ScheduleEntryAtom(labwork: SesameLabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: Room, supervisor: Set[User], group: Group, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object Schedule extends UriGenerator[Schedule] with JsonSerialisation[Schedule, Schedule, ScheduleAtom] {

  lazy val empty = Schedule(UUID.randomUUID, Set.empty[ScheduleEntry])

  override def base: String = "schedules"

  override implicit def reads: Reads[Schedule] = Json.reads[Schedule]

  override implicit def writes: Writes[Schedule] = Json.writes[Schedule]

  override implicit def writesAtom: Writes[ScheduleAtom] = ScheduleAtom.writesAtom
}

object ScheduleAtom {

  implicit def writesAtom: Writes[ScheduleAtom] = (
    (JsPath \ "labwork").write[SesameLabworkAtom] and
      (JsPath \ "entries").writeSet[ScheduleEntryAtom] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ScheduleAtom.unapply))
}

object ScheduleEntry extends UriGenerator[ScheduleEntry] with JsonSerialisation[ScheduleEntry, ScheduleEntry, ScheduleEntryAtom] {

  override implicit def reads: Reads[ScheduleEntry] = Json.reads[ScheduleEntry]

  override implicit def writes: Writes[ScheduleEntry] = Json.writes[ScheduleEntry]

  override implicit def writesAtom: Writes[ScheduleEntryAtom] = ScheduleEntryAtom.writesAtom

  override def base: String = "scheduleEntry"
}

object ScheduleEntryAtom {

  implicit def writesAtom: Writes[ScheduleEntryAtom] = (
    (JsPath \ "labwork").write[SesameLabworkAtom] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "group").write[Group] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ScheduleEntryAtom.unapply))
}
