package models.labwork

import java.util.UUID

import controllers.crud.JsonSerialisation
import models.users.Student
import models.{Room, UniqueEntity, UriGenerator}
import org.joda.time.{LocalDate, LocalTime}
import play.api.libs.json.{Format, Json, Reads, Writes}

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
case class ReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int)

case class Rescheduled(date: LocalDate, start: LocalTime, end: LocalTime, room: UUID)

/**
  * Atomic representation of a report card entry
  */

case class ReportCardEntryAtom(student: Student, labwork: Labwork, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: Room, entryTypes: Set[ReportCardEntryType], rescheduled: Option[Rescheduled], id: UUID)

object ReportCardEntry extends UriGenerator[ReportCardEntry] with JsonSerialisation[ReportCardEntry, ReportCardEntry] {

  import Labwork.format

  override def base: String = "reportCardEntries"

  implicit def atomicWrites = Json.writes[ReportCardEntryAtom]

  implicit def atomicFormat: Format[ReportCardEntryAtom] = Json.format[ReportCardEntryAtom]

  override implicit def reads: Reads[ReportCardEntry] = Json.reads[ReportCardEntry]

  override implicit def writes: Writes[ReportCardEntry] = Json.writes[ReportCardEntry]
}

object ReportCardEntryType extends UriGenerator[ReportCardEntryType] with JsonSerialisation[ReportCardEntryType, ReportCardEntryType] {

  def Attendance = ReportCardEntryType(AssignmentEntryType.Attendance.entryType)
  def Certificate = ReportCardEntryType(AssignmentEntryType.Certificate.entryType)
  def Bonus = ReportCardEntryType(AssignmentEntryType.Bonus.entryType)
  def Supplement = ReportCardEntryType(AssignmentEntryType.Supplement.entryType)

  def all = Set(Attendance, Certificate, Bonus, Supplement)

  override def base: String = "reportCardEntryTypes"

  override implicit def reads: Reads[ReportCardEntryType] = Json.reads[ReportCardEntryType]

  override implicit def writes: Writes[ReportCardEntryType] = Json.writes[ReportCardEntryType]
}

object Rescheduled extends JsonSerialisation[Rescheduled, Rescheduled] {

  override implicit def reads: Reads[Rescheduled] = Json.reads[Rescheduled]

  override implicit def writes: Writes[Rescheduled] = Json.writes[Rescheduled]
}