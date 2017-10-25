package models

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax.unlift
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import play.api.libs.functional.syntax._
import models.LwmDateTime._

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
  def writes: Writes[ReportCardRescheduled] = new Writes[ReportCardRescheduled] {
    override def writes(r: ReportCardRescheduled) = r match {
      case rescheduled: PostgresReportCardRescheduled => Json.toJson(rescheduled)(PostgresReportCardRescheduled.writes)
      case rescheduledAtom: PostgresReportCardRescheduledAtom => Json.toJson(rescheduledAtom)(PostgresReportCardRescheduledAtom.writesAtom)
    }
  }
}

object PostgresReportCardRescheduled extends JsonSerialisation[PostgresReportCardRescheduledProtocol, PostgresReportCardRescheduled, PostgresReportCardRescheduledAtom] {
  override implicit def reads: Reads[PostgresReportCardRescheduledProtocol] = Json.reads[PostgresReportCardRescheduledProtocol]

  override implicit def writes: Writes[PostgresReportCardRescheduled] = Json.writes[PostgresReportCardRescheduled]

  override implicit def writesAtom: Writes[PostgresReportCardRescheduledAtom] = PostgresReportCardRescheduledAtom.writesAtom
}

object PostgresReportCardRescheduledAtom {
  implicit def writesAtom: Writes[PostgresReportCardRescheduledAtom] = (
    (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[PostgresRoom](PostgresRoom.writes) and
      (JsPath \ "reason").writeNullable[String] and
      (JsPath \ "id").write[UUID]
    ) (unlift(PostgresReportCardRescheduledAtom.unapply))
}