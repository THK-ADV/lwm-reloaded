package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax.{unlift, _}
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.LwmDateTime._

case class SesameRescheduled(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID)

case class SesameRescheduledAtom(date: LocalDate, start: LocalTime, end: LocalTime, room: SesameRoom)

// POSTGRES

trait ReportCardRescheduled extends UniqueEntity

case class PostgresReportCardRescheduled(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, reason: Option[String] = None, id: UUID = UUID.randomUUID) extends ReportCardRescheduled

case class PostgresReportCardRescheduledProtocol(reportCardEntry: UUID, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, reason: Option[String] = None)

case class PostgresReportCardRescheduledAtom(date: LocalDate, start: LocalTime, end: LocalTime, room: PostgresRoom, reason: Option[String], id: UUID) extends ReportCardRescheduled

case class ReportCardRescheduledDb(reportCardEntry: UUID, date: Date, start: Time, end: Time, room: UUID, reason: Option[String] = None, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toLwmModel = PostgresReportCardRescheduled(date.localDate, start.localTime, end.localTime, room, reason, id)

  override def equals(that: scala.Any) = that match {
    case ReportCardRescheduledDb(rc, dt, st, et, r, rs, _, _, i) =>
      rc == reportCardEntry &&
        dt.localDate.isEqual(date.localDate) &&
        st.localTime.isEqual(start.localTime) &&
        et.localTime.isEqual(end.localTime) &&
        r == room &&
        rs == reason &&
        i == id
  }
}

object ReportCardRescheduled {

  implicit val writes: Writes[ReportCardRescheduled] = new Writes[ReportCardRescheduled] {
    override def writes(r: ReportCardRescheduled) = r match {
      case normal: PostgresReportCardRescheduled => Json.toJson(normal)(PostgresReportCardRescheduled.writes)
      case atom: PostgresReportCardRescheduledAtom => Json.toJson(atom)(PostgresReportCardRescheduledAtom.writes)
    }
  }
}

object PostgresReportCardRescheduled {
  implicit val writes: Writes[PostgresReportCardRescheduled] = Json.writes[PostgresReportCardRescheduled]
}

object PostgresReportCardRescheduledProtocol {
  implicit val reads: Reads[PostgresReportCardRescheduledProtocol] = Json.reads[PostgresReportCardRescheduledProtocol]
}

object PostgresReportCardRescheduledAtom {

  implicit val writes: Writes[PostgresReportCardRescheduledAtom] = (
    (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[PostgresRoom](PostgresRoom.writes) and
      (JsPath \ "reason").writeNullable[String] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardRescheduledAtom.unapply))
}