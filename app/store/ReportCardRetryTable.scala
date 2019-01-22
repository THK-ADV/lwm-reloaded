package store

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models.{ReportCardRetry, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

class ReportCardRetryTable(tag: Tag) extends Table[ReportCardRetryDb](tag, "REPORT_CARD_RETRY") with UniqueTable with DateStartEndTable with RoomIdTable with ReportCardEntryIdTable {
  def reason = column[Option[String]]("REASON")

  override def * = (reportCardEntry, date, start, end, room, reason, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Date, Time, Time, UUID, Option[String], Timestamp, Option[Timestamp], UUID)) => ReportCardRetryDb = {
    case (reportCardEntry, date, start, end, room, reason, lastModified, invalidated, id) =>
      ReportCardRetryDb(reportCardEntry, date, start, end, room, Set.empty, reason, lastModified, invalidated, id)
  }

  def unmapRow: ReportCardRetryDb => Option[(UUID, Date, Time, Time, UUID, Option[String], Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.reportCardEntry, entry.date, entry.start, entry.end, entry.room, entry.reason, entry.lastModified, entry.invalidated, entry.id))
  }
}

case class ReportCardRetryDb(reportCardEntry: UUID, date: Date, start: Time, end: Time, room: UUID, entryTypes: Set[ReportCardEntryTypeDb], reason: Option[String] = None, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {

  override def equals(that: scala.Any) = that match {
    case ReportCardRetryDb(rc, dt, st, et, r, ts, rs, _, _, i) =>
      rc == reportCardEntry &&
        dt.localDate.isEqual(date.localDate) &&
        st.localTime.isEqual(start.localTime) &&
        et.localTime.isEqual(end.localTime) &&
        r == room &&
        ts == entryTypes &&
        rs == reason &&
        i == id
  }

  override def toUniqueEntity = ReportCardRetry(date.localDate, start.localTime, end.localTime, room, entryTypes.map(_.toUniqueEntity), reason, id)
}
