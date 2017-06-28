package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, LocalDate, LocalDateTime, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import services.ScheduleEntryG
import utils.Ops.JsPathX
import models.LwmDateTime._

case class SesameTimetable(labwork: UUID, entries: Set[SesameTimetableEntry], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID = SesameTimetable.randomUUID) extends UniqueEntity {

  import models.LwmDateTime.dateTimeOrd

  override def equals(that: scala.Any): Boolean = that match {
    case SesameTimetable(l2, e2, s2, bl2, _, id2) =>
      l2 == labwork &&
        e2 == entries &&
        s2.isEqual(start) &&
        bl2.toVector.sorted.zip(localBlacklist.toVector.sorted).forall(d => d._1.isEqual(d._2)) &&
        id2 == id
    case _ => false
  }
}

case class SesameTimetableEntry(supervisor: Set[UUID], room: UUID, dayIndex: Int, start: LocalTime, end: LocalTime) {

  override def equals(that: scala.Any): Boolean = that match {
    case SesameTimetableEntry(sup2, room2, dayIndex2, start2, end2) =>
      supervisor == sup2 && room == room2 && dayIndex2 == dayIndex && start2.isEqual(start) && end2.isEqual(end)
    case _ => false
  }
}

case class SesameTimetableProtocol(labwork: UUID, entries: Set[SesameTimetableEntry], start: LocalDate, localBlacklist: Set[String])

case class SesameTimetableAtom(labwork: SesameLabwork, entries: Set[SesameTimetableEntryAtom], start: LocalDate, localBlacklist: Set[DateTime], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class SesameTimetableEntryAtom(supervisor: Set[User], room: SesameRoom, dayIndex: Int, start: LocalTime, end: LocalTime)

/**
  * Helper
  */

case class TimetableDateEntry(weekday: Weekday, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, supervisor: Set[UUID])

object SesameTimetable extends UriGenerator[SesameTimetable] with JsonSerialisation[SesameTimetableProtocol, SesameTimetable, SesameTimetableAtom] {

  override def base: String = "timetables"

  override implicit def reads: Reads[SesameTimetableProtocol] = Json.reads[SesameTimetableProtocol]

  override implicit def writes: Writes[SesameTimetable] = (
    (JsPath \ "labwork").write[UUID] and
      (JsPath \ "entries").writeSet[SesameTimetableEntry] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameTimetable.unapply))

  override implicit def writesAtom: Writes[SesameTimetableAtom] = SesameTimetableAtom.writesAtom
}

object SesameTimetableAtom {

  implicit def writesAtom: Writes[SesameTimetableAtom] = (
    (JsPath \ "labwork").write[SesameLabwork] and
      (JsPath \ "entries").writeSet[SesameTimetableEntryAtom] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(SesameTimetableAtom.unapply))
}

object SesameTimetableEntry extends JsonSerialisation[SesameTimetableEntry, SesameTimetableEntry, SesameTimetableEntryAtom] {

  implicit val dateOrd = LwmDateTime.localDateOrd

  implicit val timeOrd = LwmDateTime.localTimeOrd

  override implicit def reads: Reads[SesameTimetableEntry] = Json.reads[SesameTimetableEntry]

  override implicit def writes: Writes[SesameTimetableEntry] = Json.writes[SesameTimetableEntry]

  override implicit def writesAtom: Writes[SesameTimetableEntryAtom] = SesameTimetableEntryAtom.writesAtom
}

object SesameTimetableEntryAtom {
  implicit def writesAtom: Writes[SesameTimetableEntryAtom] = (
    (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "room").write[SesameRoom](SesameRoom.writes) and
      (JsPath \ "dayIndex").write[Int] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime]
    ) (unlift(SesameTimetableEntryAtom.unapply))
}

object TimetableDateEntry {

  def toLocalDateTime(entry: TimetableDateEntry): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }

  def toLocalDateTime(entry: ScheduleEntryG): LocalDateTime = {
    entry.date.toLocalDateTime(entry.start)
  }
}

// POSTGRES

sealed trait Timetable extends UniqueEntity

case class PostgresTimetable(labwork: UUID, entries: Set[PostgresTimetableEntry], start: LocalDate, localBlacklist: Set[UUID], id: UUID = UUID.randomUUID) extends Timetable {
  override def equals(that: scala.Any) = that match {
    case PostgresTimetable(l, e, s, lb, i) =>
      l == labwork &&
      e == entries &&
      s.isEqual(start) &&
      lb == localBlacklist &&
      i == id
    case _ => false
  }
}

case class PostgresTimetableEntry(supervisor: Set[UUID], room: UUID, dayIndex: Int, start: LocalTime, end: LocalTime) {
  override def equals(that: scala.Any) = that match {
    case PostgresTimetableEntry(s, r, d, st, et) =>
      s == supervisor &&
      r == room &&
      d == dayIndex &&
      st.isEqual(start) &&
      et.isEqual(end)
    case _ => false
  }
}

case class PostgresTimetableProtocol(labwork: UUID, entries: Set[PostgresTimetableEntry], start: LocalDate, localBlacklist: Set[UUID])

case class PostgresTimetableAtom(labwork: PostgresLabwork, entries: Set[PostgresTimetableEntryAtom], start: LocalDate, localBlacklist: Set[PostgresBlacklist], id: UUID = UUID.randomUUID) extends Timetable

case class PostgresTimetableEntryAtom(supervisor: Set[User], room: PostgresRoom, dayIndex: Int, start: LocalTime, end: LocalTime)

case class TimetableDb(labwork: UUID, entries: Set[PostgresTimetableEntry], start: Date, localBlacklist: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresTimetable(labwork, entries, start.localDate, localBlacklist, id)
}

case class TimetableEntryDb(timetable: UUID, room: UUID, supervisor: Set[UUID], dayIndex: Int, start: Time, end: Time, id: UUID = UUID.randomUUID) extends UniqueEntity {
  def toTimetableEntry = PostgresTimetableEntry(supervisor, room, dayIndex, start.localTime, end.localTime)
}

case class TimetableEntrySupervisor(timetableEntry: UUID, supervisor: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity

case class TimetableBlacklist(timetable: UUID, blacklist: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity

object TimetableDb {
  def from(p: PostgresTimetableProtocol, existing: Option[UUID]) = {
    TimetableDb(p.labwork, p.entries, p.start.sqlDate, p.localBlacklist, id = existing getOrElse UUID.randomUUID)
  }
}

object PostgresTimetable extends JsonSerialisation[PostgresTimetableProtocol, PostgresTimetable, PostgresTimetableAtom] {

  override implicit def reads: Reads[PostgresTimetableProtocol] = Json.reads[PostgresTimetableProtocol]

  override implicit def writes: Writes[PostgresTimetable] = Json.writes[PostgresTimetable]

  override implicit def writesAtom: Writes[PostgresTimetableAtom] = PostgresTimetableAtom.writesAtom
}

object PostgresTimetableEntry extends JsonSerialisation[PostgresTimetableEntry, PostgresTimetableEntry, PostgresTimetableEntryAtom] {

  override implicit def reads: Reads[PostgresTimetableEntry] = Json.reads[PostgresTimetableEntry]

  override implicit def writes: Writes[PostgresTimetableEntry] = Json.writes[PostgresTimetableEntry]

  override implicit def writesAtom: Writes[PostgresTimetableEntryAtom] = PostgresTimetableEntryAtom.writesAtom
}

object PostgresTimetableEntryAtom {

  implicit def writesAtom: Writes[PostgresTimetableEntryAtom] = (
    (JsPath \ "supervisor").writeSet[User] and
      (JsPath \ "room").write[PostgresRoom](PostgresRoom.writes) and
      (JsPath \ "dayIndex").write[Int] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime]
    ) (unlift(PostgresTimetableEntryAtom.unapply))
}

object PostgresTimetableAtom {

  implicit def writesAtom: Writes[PostgresTimetableAtom] = (
    (JsPath \ "labwork").write[PostgresLabwork](PostgresLabwork.writes) and
      (JsPath \ "entries").writeSet[PostgresTimetableEntryAtom] and
      (JsPath \ "start").write[LocalDate] and
      (JsPath \ "localBlacklist").writeSet[PostgresBlacklist](PostgresBlacklist.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresTimetableAtom.unapply))
}

object Timetable {

  implicit val writes: Writes[Timetable] = new Writes[Timetable] {
    override def writes(t: Timetable) = t match {
      case timetable: PostgresTimetable => Json.toJson(timetable)(PostgresTimetable.writes)
      case atom: PostgresTimetableAtom => Json.toJson(atom)(PostgresTimetableAtom.writesAtom)
    }
  }
}
