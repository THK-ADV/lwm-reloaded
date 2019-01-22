package store

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models.{ReportCardRescheduled, UniqueDbEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

class ReportCardRescheduledTable(tag: Tag) extends Table[ReportCardRescheduledDb](tag, "REPORT_CARD_RESCHEDULED") with UniqueTable with DateStartEndTable with RoomIdTable with ReportCardEntryIdTable {
  def reason = column[Option[String]]("REASON")

  override def * = (reportCardEntry, date, start, end, room, reason, lastModified, invalidated, id) <> ((ReportCardRescheduledDb.apply _).tupled, ReportCardRescheduledDb.unapply)
}

case class ReportCardRescheduledDb(reportCardEntry: UUID, date: Date, start: Time, end: Time, room: UUID, reason: Option[String] = None, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = ReportCardRescheduled(date.localDate, start.localTime, end.localTime, room, reason, id)

  override def equals(that: scala.Any) = that match {
    case ReportCardRescheduledDb(rc, dt, st, et, r, rs, _, _, i) =>
      rc == reportCardEntry &&
        dt.localDate.isEqual(date.localDate) &&
        st.localTime.isEqual(start.localTime) &&
        et.localTime.isEqual(end.localTime) &&
        r == room &&
        rs == reason &&
        i == id
  }
}
