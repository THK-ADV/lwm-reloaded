package database

import java.sql.Timestamp
import java.util.UUID

import models.{ReportCardEntryType, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

class ReportCardEntryTypeTable(tag: Tag) extends Table[ReportCardEntryTypeDb](tag, "REPORT_CARD_ENTRY_TYPE") with UniqueTable with EntryTypeTable {
  def bool = column[Option[Boolean]]("BOOL")

  def int = column[Int]("INT")

  def reportCardEntry = column[UUID]("REPORT_CARD_ENTRY")

  def reportCardEntryFk = foreignKey("REPORT_CARD_ENTRY_fkey", reportCardEntry, TableQuery[ReportCardEntryTable])(_.id)

  def joinReportCardEntry(f: ReportCardEntryTable => Rep[Boolean]) = reportCardEntryFk.filter(e => e.isValid && e.id === reportCardEntry && f(e))

  override def * = (reportCardEntry, entryType, bool, int, lastModified, invalidated, id) <> ((ReportCardEntryTypeDb.apply _).tupled, ReportCardEntryTypeDb.unapply)
}

case class ReportCardEntryTypeDb(
  reportCardEntry: UUID,
  entryType: String,
  bool: Option[Boolean] = None,
  int: Int = 0,
  lastModified: Timestamp = DateTime.now.timestamp,
  invalidated: Option[Timestamp] = None,
  id: UUID = UUID.randomUUID
) extends UniqueDbEntity {

  override def toUniqueEntity = ReportCardEntryType(entryType, bool, int, id)

  override def equals(obj: Any) = obj match {
    case ReportCardEntryTypeDb(e, t, _bool, _int, _, _, i) =>
      e == reportCardEntry &&
        t == entryType &&
        _bool == bool &&
        _int == int &&
        i == id
    case _ => false
  }
}