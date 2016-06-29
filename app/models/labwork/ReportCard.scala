package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{Room, UniqueEntity, UriGenerator}
import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.json._

case class ReportCardEntry(student: UUID, labwork: UUID, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[ReportCardEntryType], rescheduled: Option[Rescheduled] = None, id: UUID = ReportCardEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case ReportCardEntry(s, l, la, d, st, e, r, et, rs, i) =>
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

case class ReportCardEntryType(entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = ReportCardEntryType.randomUUID) extends UniqueEntity
// TODO make them repo ready
case class ReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, id: UUID = ReportCardEvaluation.randomUUID) extends UniqueEntity

case class Rescheduled(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID)

/**
  * Atomic
  */

case class ReportCardEntryAtom(student: Student, labwork: Labwork, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: Room, entryTypes: Set[ReportCardEntryType], rescheduled: Option[RescheduledAtom], id: UUID) extends UniqueEntity

case class RescheduledAtom(date: LocalDate, start: LocalTime, end: LocalTime, room: Room)

case class ReportCardEvaluationAtom(student: Student, labwork: Labwork, label: String, bool: Boolean, int: Int, id: UUID) extends UniqueEntity

object ReportCardEntry extends UriGenerator[ReportCardEntry] with JsonSerialisation[ReportCardEntry, ReportCardEntry, ReportCardEntryAtom] {

  override def base: String = "reportCardEntries"

  override implicit def reads: Reads[ReportCardEntry] = Json.reads[ReportCardEntry]

  override implicit def writes: Writes[ReportCardEntry] = Json.writes[ReportCardEntry]

  override implicit def writesAtom: Writes[ReportCardEntryAtom] = Writes[ReportCardEntryAtom] { entry =>
    val json = Json.obj(
      "student" -> Json.toJson(entry.student),
      "labwork" -> Json.toJson(entry.labwork),
      "label" -> entry.label,
      "date" -> entry.date.toString,
      "start" -> entry.start.toString,
      "end" -> entry.end.toString,
      "room" -> Json.toJson(entry.room),
      "entryTypes" -> Json.toJson(entry.entryTypes),
      "id" -> entry.id.toString
    )

    entry.rescheduled.fold(json)(rs =>
      json + ("rescheduled" -> Json.obj(
        "date" -> rs.date.toString,
        "start" -> rs.start.toString,
        "end" -> rs.end.toString,
        "room" -> Json.toJson(rs.room)))
    )
  }
}

object ReportCardEntryType extends UriGenerator[ReportCardEntryType] with JsonSerialisation[ReportCardEntryType, ReportCardEntryType, ReportCardEntryType] {

  def Attendance = ReportCardEntryType(AssignmentEntryType.Attendance.entryType)
  def Certificate = ReportCardEntryType(AssignmentEntryType.Certificate.entryType)
  def Bonus = ReportCardEntryType(AssignmentEntryType.Bonus.entryType)
  def Supplement = ReportCardEntryType(AssignmentEntryType.Supplement.entryType)

  def all = Set(Attendance, Certificate, Bonus, Supplement)

  override def base: String = "reportCardEntryTypes"

  override implicit def reads: Reads[ReportCardEntryType] = Json.reads[ReportCardEntryType]

  override implicit def writes: Writes[ReportCardEntryType] = Json.writes[ReportCardEntryType]

  override def writesAtom: Writes[ReportCardEntryType] = writes
}

object ReportCardEvaluation extends UriGenerator[ReportCardEvaluation] with JsonSerialisation[ReportCardEvaluation, ReportCardEvaluation, ReportCardEvaluationAtom] {

  override def base: String = "reportCardEvaluation"

  override implicit def reads: Reads[ReportCardEvaluation] = Json.reads[ReportCardEvaluation]

  override implicit def writes: Writes[ReportCardEvaluation] = Json.writes[ReportCardEvaluation]

  override implicit def writesAtom: Writes[ReportCardEvaluationAtom] = Json.writes[ReportCardEvaluationAtom]

}

object Rescheduled extends JsonSerialisation[Rescheduled, Rescheduled, RescheduledAtom] {

  override implicit def reads: Reads[Rescheduled] = Json.reads[Rescheduled]

  override implicit def writes: Writes[Rescheduled] = Json.writes[Rescheduled]

  override implicit def writesAtom: Writes[RescheduledAtom] = Json.writes[RescheduledAtom]
}