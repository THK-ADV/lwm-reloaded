package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import org.joda.time.{LocalTime, LocalDate}
import play.api.libs.json.{Json, Reads, Writes}

case class ReportCard(student: UUID, labwork: UUID, entries: Set[ReportCardEntry], id: UUID = ReportCard.randomUUID) extends UniqueEntity

case class ReportCardEntry(index: Int, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[ReportCardEntryType], id: UUID = ReportCardEntry.randomUUID) extends UniqueEntity

case class ReportCardEntryType(entryType: String, bool: Boolean = false, int: Int = 0, id: UUID = ReportCardEntryType.randomUUID) extends UniqueEntity

// TODO make them repo ready
case class ReportCardEvaluation(student: UUID, labwork: UUID, label: String, bool: Boolean, int: Int)

object ReportCard extends UriGenerator[ReportCard] with JsonSerialisation[ReportCard, ReportCard] {

  lazy val empty = ReportCard(UUID.randomUUID(), UUID.randomUUID(), Set.empty[ReportCardEntry])

  override def base: String = "reportCards"

  override implicit def reads: Reads[ReportCard] = Json.reads[ReportCard]

  override implicit def writes: Writes[ReportCard] = Json.writes[ReportCard]
}

object ReportCardEntry extends UriGenerator[ReportCardEntry] with JsonSerialisation[ReportCardEntry, ReportCardEntry] {

  override def base: String = "reportCardEntries"

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