package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime._
import utils.Ops.JsPathX

sealed trait ScheduleEntry extends UniqueEntity

case class PostgresScheduleEntry(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID, id: UUID = UUID.randomUUID) extends ScheduleEntry

case class PostgresScheduleEntryAtom(labwork: PostgresLabworkAtom, start: LocalTime, end: LocalTime, date: LocalDate, room: PostgresRoom, supervisor: Set[User], group: PostgresGroup, id: UUID) extends ScheduleEntry

case class PostgresScheduleEntryProtocol(labwork: UUID, start: LocalTime, end: LocalTime, date: LocalDate, room: UUID, supervisor: Set[UUID], group: UUID)

object PostgresScheduleEntry {
  implicit val writes: Writes[PostgresScheduleEntry] = Json.writes[PostgresScheduleEntry]
}

object PostgresScheduleEntryProtocol {
  implicit val reads: Reads[PostgresScheduleEntryProtocol] = Json.reads[PostgresScheduleEntryProtocol]
}

object PostgresScheduleEntryAtom {

  implicit val writes: Writes[PostgresScheduleEntryAtom] = (
    (JsPath \ "labwork").write[PostgresLabworkAtom](PostgresLabworkAtom.writes) and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "room").write[PostgresRoom](PostgresRoom.writes) and
      (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "group").write[PostgresGroup](PostgresGroup.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresScheduleEntryAtom.unapply))
}

object ScheduleEntry {

  implicit val writes: Writes[ScheduleEntry] = new Writes[ScheduleEntry] {
    override def writes(s: ScheduleEntry) = s match {
      case scheduleEntry: PostgresScheduleEntry => Json.toJson(scheduleEntry)(PostgresScheduleEntry.writes)
      case scheduleEntryAtom: PostgresScheduleEntryAtom => Json.toJson(scheduleEntryAtom)(PostgresScheduleEntryAtom.writes)
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