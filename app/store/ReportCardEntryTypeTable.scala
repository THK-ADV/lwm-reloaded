package store

import java.sql.Timestamp
import java.util.UUID

import models.{ReportCardEntryType, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

class ReportCardEntryTypeTable(tag: Tag) extends Table[ReportCardEntryTypeDb](tag, "REPORT_CARD_ENTRY_TYPE") with UniqueTable with EntryTypeTable {
  def bool = column[Option[Boolean]]("BOOL")

  def int = column[Int]("INT")

  // reportCardEntries types can either be created from reportCardEntry or reportCardRetry. thus both ids are optional
  def reportCardEntry = column[Option[UUID]]("REPORT_CARD_ENTRY")

  def reportCardRetry = column[Option[UUID]]("REPORT_CARD_RETRY")

  def joinReportCardEntry = TableQuery[ReportCardEntryTable].filter(_.id === reportCardEntry)

  def joinReportCardRetry = TableQuery[ReportCardRetryTable].filter(_.id === reportCardRetry)

  def reportCardEntryFk = foreignKey("REPORT_CARD_ENTRY_fkey", reportCardEntry, TableQuery[ReportCardEntryTable])(_.id.?)

  def reportCardRetryFk = foreignKey("REPORT_CARD_RETRY_fkey", reportCardRetry, TableQuery[ReportCardRetryTable])(_.id.?)

  override def * = (reportCardEntry, reportCardRetry, entryType, bool, int, lastModified, invalidated, id) <> ((ReportCardEntryTypeDb.apply _).tupled, ReportCardEntryTypeDb.unapply)
}

case class ReportCardEntryTypeDb(
  reportCardEntry: Option[UUID],
  reportCardRetry: Option[UUID],
  entryType: String,
  bool: Option[Boolean] = None,
  int: Int = 0,
  lastModified: Timestamp = DateTime.now.timestamp,
  invalidated: Option[Timestamp] = None,
  id: UUID = UUID.randomUUID
) extends UniqueDbEntity {
  override def toUniqueEntity = ReportCardEntryType(entryType, bool, int, id)
}