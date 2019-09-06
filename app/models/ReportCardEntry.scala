package models

import java.util.UUID

import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX
import utils.date.DateTimeJsonFormatter._

sealed trait ReportCardEntryLike extends UniqueEntity {
  def labworkId: UUID

  def date: LocalDate

  def start: LocalTime

  def end: LocalTime
}

case class ReportCardEntry(
  student: UUID,
  labwork: UUID,
  label: String,
  date: LocalDate,
  start: LocalTime,
  end: LocalTime,
  room: UUID,
  entryTypes: Set[ReportCardEntryType],
  assignmentIndex: Int,
  rescheduled: Option[ReportCardRescheduled] = None,
  retry: Option[ReportCardRetry] = None,
  id: UUID = UUID.randomUUID
) extends ReportCardEntryLike {
  override def labworkId = labwork
}

case class ReportCardEntryProtocol(
  student: UUID,
  labwork: UUID,
  label: String,
  date: LocalDate,
  start: LocalTime,
  end: LocalTime,
  room: UUID
)

case class ReportCardEntryAtom(
  student: User,
  labwork: Labwork,
  label: String,
  date: LocalDate,
  start: LocalTime,
  end: LocalTime,
  room: Room,
  entryTypes: Set[ReportCardEntryType],
  assignmentIndex: Int,
  rescheduled: Option[ReportCardRescheduledAtom],
  retry: Option[ReportCardRetryAtom],
  id: UUID
) extends ReportCardEntryLike {
  override def labworkId = labwork.id
}

object ReportCardEntry {

  implicit val writes: Writes[ReportCardEntry] = (
    (JsPath \ "student").write[UUID] and
      (JsPath \ "labwork").write[UUID] and
      (JsPath \ "label").write[String] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[UUID] and
      (JsPath \ "entryTypes").writeSet[ReportCardEntryType](ReportCardEntryType.writes) and
      (JsPath \ "assignmentIndex").write[Int] and
      (JsPath \ "rescheduled").writeNullable[ReportCardRescheduled](ReportCardRescheduled.writes) and
      (JsPath \ "retry").writeNullable[ReportCardRetry](ReportCardRetry.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEntry.unapply))
}

object ReportCardEntryProtocol {
  implicit val reads: Reads[ReportCardEntryProtocol] = Json.reads[ReportCardEntryProtocol]
}

object ReportCardEntryAtom {

  implicit val writes: Writes[ReportCardEntryAtom] = (
    (JsPath \ "student").write[User] and
      (JsPath \ "labwork").write[Labwork](Labwork.writes) and
      (JsPath \ "label").write[String] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "entryTypes").writeSet[ReportCardEntryType](ReportCardEntryType.writes) and
      (JsPath \ "assignmentIndex").write[Int] and
      (JsPath \ "rescheduled").writeNullable[ReportCardRescheduledAtom](ReportCardRescheduledAtom.writes) and
      (JsPath \ "retry").writeNullable[ReportCardRetryAtom](ReportCardRetryAtom.writes) and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEntryAtom.unapply))
}

object ReportCardEntryLike {

  implicit val writes: Writes[ReportCardEntryLike] = {
    case normal: ReportCardEntry => Json.toJson(normal)(ReportCardEntry.writes)
    case atom: ReportCardEntryAtom => Json.toJson(atom)(ReportCardEntryAtom.writes)
  }
}