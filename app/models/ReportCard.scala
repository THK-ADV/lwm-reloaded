package models

import java.util.UUID

import controllers.crud.JsonSerialisation
import org.joda.time.{LocalTime, LocalDate}
import play.api.libs.json.{Json, Reads, Writes}

case class ReportCard(student: UUID, labwork: UUID, entries: Set[ReportCardEntry], id: UUID = ReportCard.randomUUID) extends UniqueEntity

case class ReportCardEntry(index: Int, label: String, date: LocalDate, start: LocalTime, end: LocalTime, room: UUID, entryTypes: Set[AssignmentEntryType], id: UUID = ReportCardEntry.randomUUID) extends UniqueEntity

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