package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.LwmDateTime._
import utils.Ops.JsPathX

case class SesameReportCardEntry(student: UUID, labwork: UUID, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[SesameReportCardEntryType], rescheduled: Option[SesameRescheduled] = None, invalidated: Option[DateTime] = None, id: UUID = SesameReportCardEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case SesameReportCardEntry(s, l, la, d, st, e, r, et, rs, _, i) =>
      s == student &&
        l == labwork &&
        la == label &&
        d.isEqual(date) &&
        st.isEqual(start) &&
        e.isEqual(end) &&
        r == room &&
        et == entryTypes &&
        rs == rescheduled &&
        i == id
    case None => false
  }
}

case class SesameReportCardEntryAtom(student: SesameStudent, labwork: SesameLabwork, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: SesameRoom, entryTypes: Set[SesameReportCardEntryType], rescheduled: Option[SesameRescheduledAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

object SesameReportCardEntry extends UriGenerator[SesameReportCardEntry] {

  override def base: String = "reportCardEntries"
}

// POSTGRES

sealed trait ReportCardEntry extends UniqueEntity

case class PostgresReportCardEntry(student: UUID, labwork: UUID, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[PostgresReportCardEntryType], rescheduled: Option[PostgresReportCardRescheduled] = None, retry: Option[PostgresReportCardRetry] = None, id: UUID = UUID.randomUUID) extends ReportCardEntry

case class PostgresReportCardEntryProtocol(student: UUID, labwork: UUID, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID)

case class PostgresReportCardEntryAtom(student: User, labwork: PostgresLabwork, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: PostgresRoom, entryTypes: Set[PostgresReportCardEntryType], rescheduled: Option[PostgresReportCardRescheduledAtom], retry: Option[PostgresReportCardRetryAtom], id: UUID) extends ReportCardEntry

// DB

case class ReportCardEntryDb(student: UUID, labwork: UUID, label: String, date: Date, start: Time, end: Time, room: UUID, entryTypes: Set[ReportCardEntryTypeDb], rescheduled: Option[ReportCardRescheduledDb] = None, retry: Option[ReportCardRetryDb] = None, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {

  override def toLwmModel = PostgresReportCardEntry(
    student,
    labwork,
    label,
    date.localDate,
    start.localTime,
    end.localTime,
    room,
    entryTypes.map(_.toLwmModel),
    rescheduled.map(_.toLwmModel),
    retry.map(_.toLwmModel),
    id
  )

  override def equals(that: scala.Any) = that match {
    case ReportCardEntryDb(s, l, lb, dt, st, et, r, ts, rs, rt, _, _, i) =>
      s == student &&
        l == labwork &&
        lb == label &&
        dt.localDate.isEqual(date.localDate) &&
        st.localTime.isEqual(start.localTime) &&
        et.localTime.isEqual(end.localTime) &&
        r == room &&
        ts == entryTypes &&
        rs == rescheduled &&
        rt == retry &&
        i == id
    case _ => false
  }
}

// COMPS

object PostgresReportCardEntry {

  implicit val writes: Writes[PostgresReportCardEntry] = (
    (JsPath \ "student").write[UUID] and
      (JsPath \ "labwork").write[UUID] and
      (JsPath \ "label").write[String] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[UUID] and
      (JsPath \ "entryTypes").writeSet[PostgresReportCardEntryType](PostgresReportCardEntryType.writes) and
      (JsPath \ "rescheduled").writeNullable[PostgresReportCardRescheduled](PostgresReportCardRescheduled.writes) and
      (JsPath \ "retry").writeNullable[PostgresReportCardRetry](PostgresReportCardRetry.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardEntry.unapply))
}

object PostgresReportCardEntryProtocol {
  implicit val reads: Reads[PostgresReportCardEntryProtocol] = Json.reads[PostgresReportCardEntryProtocol]
}

object PostgresReportCardEntryAtom {

  implicit val writes: Writes[PostgresReportCardEntryAtom] = (
    (JsPath \ "student").write[User] and
      (JsPath \ "labwork").write[PostgresLabwork](PostgresLabwork.writes) and
      (JsPath \ "label").write[String] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[PostgresRoom](PostgresRoom.writes) and
      (JsPath \ "entryTypes").writeSet[PostgresReportCardEntryType](PostgresReportCardEntryType.writes) and
      (JsPath \ "rescheduled").writeNullable[PostgresReportCardRescheduledAtom](PostgresReportCardRescheduledAtom.writes) and
      (JsPath \ "retry").writeNullable[PostgresReportCardRetryAtom](PostgresReportCardRetryAtom.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardEntryAtom.unapply))
}

object ReportCardEntry {

  implicit val writes: Writes[ReportCardEntry] = new Writes[ReportCardEntry] {
    override def writes(entry: ReportCardEntry) = entry match {
      case normal: PostgresReportCardEntry => Json.toJson(normal)(PostgresReportCardEntry.writes)
      case atom: PostgresReportCardEntryAtom => Json.toJson(atom)(PostgresReportCardEntryAtom.writes)
    }
  }
}