package models

import java.util.UUID

import controllers.JsonSerialisation
import org.joda.time.{DateTime, LocalDate, LocalTime}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import utils.Ops.JsPathX

/**
  * ReportCard
  */

case class ReportCardEntry(student: UUID, labwork: UUID, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[ReportCardEntryType], rescheduled: Option[Rescheduled] = None, invalidated: Option[DateTime] = None, id: UUID = ReportCardEntry.randomUUID) extends UniqueEntity {

  override def equals(that: scala.Any): Boolean = that match {
    case ReportCardEntry(s, l, la, d, st, e, r, et, rs, _, i) =>
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

case class ReportCardEntryType(entryType: String, bool: Boolean = false, int: Int = 0, invalidated: Option[DateTime] = None, id: UUID = ReportCardEntryType.randomUUID) extends UniqueEntity

case class ReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int, timestamp: DateTime = DateTime.now, invalidated: Option[DateTime] = None, id: UUID = ReportCardEvaluation.randomUUID) extends UniqueEntity

case class Rescheduled(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID)

/**
  * Atomic
  */

case class ReportCardEntryAtom(student: Student, labwork: Labwork, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: Room, entryTypes: Set[ReportCardEntryType], rescheduled: Option[RescheduledAtom], invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

case class RescheduledAtom(date: LocalDate, start: LocalTime, end: LocalTime, room: Room)

case class ReportCardEvaluationAtom(student: Student, labwork: LabworkAtom, label: String, bool: Boolean, int: Int, timestamp: DateTime, invalidated: Option[DateTime] = None, id: UUID) extends UniqueEntity

/**
  * Companions
  */

object ReportCardEntry extends UriGenerator[ReportCardEntry] with JsonSerialisation[ReportCardEntry, ReportCardEntry, ReportCardEntryAtom] {

  override def base: String = "reportCardEntries"

  override implicit def reads: Reads[ReportCardEntry] = Json.reads[ReportCardEntry]

  override implicit def writes: Writes[ReportCardEntry] = Json.writes[ReportCardEntry]

  override implicit def writesAtom: Writes[ReportCardEntryAtom] = ReportCardEntryAtom.writesAtom
}


object ReportCardEntryType extends UriGenerator[ReportCardEntryType] with JsonSerialisation[ReportCardEntryType, ReportCardEntryType, ReportCardEntryType] {

  def all = Set(Attendance, Certificate, Bonus, Supplement)

  def Attendance = ReportCardEntryType(AssignmentEntryType.Attendance.entryType)

  def Certificate = ReportCardEntryType(AssignmentEntryType.Certificate.entryType)

  def Bonus = ReportCardEntryType(AssignmentEntryType.Bonus.entryType)

  def Supplement = ReportCardEntryType(AssignmentEntryType.Supplement.entryType)

  override def base: String = "reportCardEntryTypes"

  override implicit def reads: Reads[ReportCardEntryType] = Json.reads[ReportCardEntryType]

  override def writesAtom: Writes[ReportCardEntryType] = writes

  override implicit def writes: Writes[ReportCardEntryType] = Json.writes[ReportCardEntryType]
}

object ReportCardEvaluation extends UriGenerator[ReportCardEvaluation] with JsonSerialisation[ReportCardEvaluation, ReportCardEvaluation, ReportCardEvaluationAtom] {

  override def base: String = "reportCardEvaluation"

  override implicit def reads: Reads[ReportCardEvaluation] = Json.reads[ReportCardEvaluation]

  override implicit def writes: Writes[ReportCardEvaluation] = (
    (JsPath \ "student").write[UUID] and
      (JsPath \ "labwork").write[UUID] and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEvaluation.unapply))

  override implicit def writesAtom: Writes[ReportCardEvaluationAtom] = ReportCardEvaluationAtom.writesAtom
}

object Rescheduled extends JsonSerialisation[Rescheduled, Rescheduled, RescheduledAtom] {

  override implicit def reads: Reads[Rescheduled] = Json.reads[Rescheduled]

  override implicit def writes: Writes[Rescheduled] = Json.writes[Rescheduled]

  override implicit def writesAtom: Writes[RescheduledAtom] = RescheduledAtom.writesAtom
}

object RescheduledAtom {

  implicit def writesAtom: Writes[RescheduledAtom] = (
    (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[Room](Room.writes)
    ) (unlift(RescheduledAtom.unapply))
}

object ReportCardEntryAtom {

  implicit def writesAtom: Writes[ReportCardEntryAtom] = (
    (JsPath \ "student").write[Student](Student.writes) and
      (JsPath \ "labwork").write[Labwork] and
      (JsPath \ "label").write[String] and
      (JsPath \ "date").write[LocalDate] and
      (JsPath \ "start").write[LocalTime] and
      (JsPath \ "end").write[LocalTime] and
      (JsPath \ "room").write[Room](Room.writes) and
      (JsPath \ "entryTypes").writeSet[ReportCardEntryType] and
      (JsPath \ "rescheduled").writeNullable[RescheduledAtom] and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEntryAtom.unapply))
}

object ReportCardEvaluationAtom {

  implicit def writesAtom: Writes[ReportCardEvaluationAtom] = (
    (JsPath \ "student").write[Student](Student.writes) and
      (JsPath \ "labwork").write[LabworkAtom] and
      (JsPath \ "label").write[String] and
      (JsPath \ "bool").write[Boolean] and
      (JsPath \ "int").write[Int] and
      (JsPath \ "timestamp").write[DateTime](LwmDateTime.writes) and
      (JsPath \ "invalidated").writeNullable[DateTime] and
      (JsPath \ "id").write[UUID]
    ) (unlift(ReportCardEvaluationAtom.unapply))
}