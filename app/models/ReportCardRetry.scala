package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import utils.date.DateTimeJsonFormatter._

sealed trait ReportCardRetryLike extends UniqueEntity

case class ReportCardRetry(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[ReportCardEntryType], reason: Option[String] = None, id: UUID = UUID.randomUUID) extends ReportCardRetryLike

case class ReportCardRetryProtocol(reportCardEntry: UUID, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[ReportCardEntryTypeProtocol], reason: Option[String])

case class ReportCardRetryAtom(date: LocalDate, start: LocalTime, end: LocalTime, room: Room, entryTypes: Set[ReportCardEntryType], reason: Option[String], id: UUID) extends ReportCardRetryLike

object ReportCardRetry {
  implicit val writes: Writes[ReportCardRetry] = Json.writes[ReportCardRetry]
}

object ReportCardRetryProtocol {
  implicit val reads: Reads[ReportCardRetryProtocol] = Json.reads[ReportCardRetryProtocol]
}

object ReportCardRetryAtom {

  implicit val writes: Writes[ReportCardRetryAtom] = (
    (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "entryTypes").writeSet[ReportCardEntryType](ReportCardEntryType.writes) and
      (JsPath \ "reason").writeNullable[String] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardRetryAtom.unapply))
}

object ReportCardRetryLike {

  implicit val writes: Writes[ReportCardRetryLike] = {
    case normal: ReportCardRetry => Json.toJson(normal)(ReportCardRetry.writes)
    case atom: ReportCardRetryAtom => Json.toJson(atom)(ReportCardRetryAtom.writes)
  }
}