package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.functional.syntax.{unlift, _}
import play.api.libs.json.{JsPath, Json, Reads, Writes}
import utils.date.DateTimeJsonFormatter._

trait ReportCardRescheduledLike extends UniqueEntity

case class ReportCardRescheduled(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, reason: Option[String] = None, id: UUID = UUID.randomUUID) extends ReportCardRescheduledLike

case class ReportCardRescheduledProtocol(reportCardEntry: UUID, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, reason: Option[String] = None)

case class ReportCardRescheduledAtom(date: LocalDate, start: LocalTime, end: LocalTime, room: Room, reason: Option[String], id: UUID) extends ReportCardRescheduledLike

object ReportCardRescheduledLike {

  implicit val writes: Writes[ReportCardRescheduledLike] = {
    case normal: ReportCardRescheduled => Json.toJson(normal)(ReportCardRescheduled.writes)
    case atom: ReportCardRescheduledAtom => Json.toJson(atom)(ReportCardRescheduledAtom.writes)
  }
}

object ReportCardRescheduled {
  implicit val writes: Writes[ReportCardRescheduled] = Json.writes[ReportCardRescheduled]
}

object ReportCardRescheduledProtocol {
  implicit val reads: Reads[ReportCardRescheduledProtocol] = Json.reads[ReportCardRescheduledProtocol]
}

object ReportCardRescheduledAtom {

  implicit val writes: Writes[ReportCardRescheduledAtom] = (
    (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "reason").writeNullable[String] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardRescheduledAtom.unapply))
}