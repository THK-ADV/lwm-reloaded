package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import utils.LwmDateTime._

case class SesameSchedule(labwork: UUID, entries: Set[SesameScheduleEntry], invalidated: Option[DateTime] = None, id: UUID = SesameSchedule.randomUUID) extends UniqueEntity

case class SesameScheduleEntry(labwork: UUID,
                               start: LocalTime,
                               end: LocalTime,
                               date: LocalDate,
                               room: UUID,
                               supervisor: Set[UUID],
                               group: UUID,
                               invalidated: Option[DateTime] = None,
                               id: UUID = SesameScheduleEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case SesameScheduleEntry(l, s, e, d, r, su, g, _, i) =>
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

case class SesameScheduleAtom(labwork: SesameLabworkAtom, entries: Set[SesameScheduleEntryAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class SesameScheduleEntryAtom(labwork: SesameLabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: SesameRoom, supervisor: Set[User], group: SesameGroup, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameSchedule extends UriGenerator[SesameSchedule] with JsonSerialisation[SesameSchedule, SesameSchedule, SesameScheduleAtom] {

  lazy val empty = SesameSchedule(UUID.randomUUID, Set.empty[SesameScheduleEntry])

  override def base: String = "schedules"

  override implicit def reads: Reads[SesameSchedule] = Json.reads[SesameSchedule]

  override implicit def writes: Writes[SesameSchedule] = Json.writes[SesameSchedule]

  override implicit def writesAtom: Writes[SesameScheduleAtom] = SesameScheduleAtom.writesAtom
}

object SesameScheduleAtom {

  implicit def writesAtom: Writes[SesameScheduleAtom] = (
    (JsPath \ "labwork").write[SesameLabworkAtom] and
      (JsPath \ "entries").writeSet[SesameScheduleEntryAtom] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameScheduleAtom.unapply))
}

object SesameScheduleEntry extends UriGenerator[SesameScheduleEntry] with JsonSerialisation[SesameScheduleEntry, SesameScheduleEntry, SesameScheduleEntryAtom] {

  override implicit def reads: Reads[SesameScheduleEntry] = Json.reads[SesameScheduleEntry]

  override implicit def writes: Writes[SesameScheduleEntry] = Json.writes[SesameScheduleEntry]

  override implicit def writesAtom: Writes[SesameScheduleEntryAtom] = SesameScheduleEntryAtom.writesAtom

  override def base: String = "scheduleEntry"
}

object SesameScheduleEntryAtom {

  implicit def writesAtom: Writes[SesameScheduleEntryAtom] = (
    (JsPath \ "labwork").write[SesameLabworkAtom] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[SesameRoom](SesameRoom.writes) and
      (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "group").write[SesameGroup] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameScheduleEntryAtom.unapply))
}

// POSTGRES

sealed trait ScheduleEntry extends UniqueEntity

case class PostgresScheduleEntry(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID, id: UUID = UUID.randomUUID) extends ScheduleEntry

case class PostgresScheduleEntryAtom(labwork: PostgresLabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: PostgresRoom, supervisor: Set[User], group: PostgresGroup, id: UUID) extends ScheduleEntry

case class PostgresScheduleEntryProtocol(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID)

object PostgresScheduleEntry extends JsonSerialisation[PostgresScheduleEntryProtocol, PostgresScheduleEntry, PostgresScheduleEntryAtom] {
  override implicit def reads = Json.reads[PostgresScheduleEntryProtocol]

  override implicit def writes = Json.writes[PostgresScheduleEntry]

  override implicit def writesAtom = PostgresScheduleEntryAtom.writesAtom
}

object PostgresScheduleEntryAtom {

  implicit def writesAtom: Writes[PostgresScheduleEntryAtom] = (
    (JsPath \ "labwork").write[PostgresLabworkAtom](PostgresLabworkAtom.writesAtom) and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[PostgresRoom](PostgresRoom.writes) and
      (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "group").write[PostgresGroup] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresScheduleEntryAtom.unapply))
}

object ScheduleEntry {
  implicit def writes: Writes[ScheduleEntry] = new Writes[ScheduleEntry] {
    override def writes(s: ScheduleEntry) = s match {
      case scheduleEntry: PostgresScheduleEntry => Json.toJson(scheduleEntry)(PostgresScheduleEntry.writes)
      case scheduleEntryAtom: PostgresScheduleEntryAtom => Json.toJson(scheduleEntryAtom)(PostgresScheduleEntryAtom.writesAtom)
    }
  }
}

// DB

case class ScheduleEntryDb(labwork: UUID, start: Time, end: Time, date: Date, room: UUID, supervisor: Set[UUID], group: UUID, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresScheduleEntry(labwork, start.localTime, end.localTime, date.localDate, room, supervisor, group, id)

  override def equals(that: scala.Any) = that match {
    case ScheduleEntryDb(l, s, e, d, r, sup, g, _, _, i) =>
      l == labwork &&
      s.localTime.isEqual(start.localTime) &&
      e.localTime.isEqual(end.localTime) &&
      d.localDate.isEqual(date.localDate) &&
      r == room &&
      sup == supervisor &&
      g == group &&
      i == id
    case _ => false
  }
}

case class ScheduleEntrySupervisor(scheduleEntry: UUID, supervisor: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity