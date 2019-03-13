package database

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models._
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

class ReportCardEntryTable(tag: Tag) extends Table[ReportCardEntryDb](tag, "REPORT_CARD_ENTRY") with UniqueTable with LabworkIdTable with LabelTable with DateStartEndTable with RoomIdTable with StudentIdTable {

  override def * = (student, labwork, label, date, start, end, room, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, String, Date, Time, Time, UUID, Timestamp, Option[Timestamp], UUID)) => ReportCardEntryDb = {
    case (student, labwork, label, date, start, end, room, lastModified, invalidated, id) =>
      ReportCardEntryDb(student, labwork, label, date, start, end, room, Set.empty, None, None, lastModified, invalidated, id)
  }

  def unmapRow: ReportCardEntryDb => Option[(UUID, UUID, String, Date, Time, Time, UUID, Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.lastModified, entry.invalidated, entry.id))
  }
}

case class ReportCardEntryDb(
  student: UUID,
  labwork: UUID,
  label: String,
  date: Date,
  start: Time,
  end: Time,
  room: UUID,
  entryTypes: Set[ReportCardEntryTypeDb],
  rescheduled: Option[ReportCardRescheduledDb] = None,
  retry: Option[ReportCardRetryDb] = None,
  lastModified: Timestamp = DateTime.now.timestamp,
  invalidated: Option[Timestamp] = None,
  id: UUID = UUID.randomUUID
) extends UniqueDbEntity {

  override def toUniqueEntity = ReportCardEntry(
    student,
    labwork,
    label,
    date.localDate,
    start.localTime,
    end.localTime,
    room,
    entryTypes.map(_.toUniqueEntity),
    rescheduled.map(_.toUniqueEntity),
    retry.map(_.toUniqueEntity),
    id
  )

  override def equals(that: scala.Any) = that match {
    case ReportCardEntryDb(s, l, lb, dt, st, et, r, ts, rs, rt, _, _, i) =>
      s == student &&
        l == labwork &&
        lb == label &&
        dt.localDate == date.localDate &&
        st.localTime == start.localTime &&
        et.localTime == end.localTime &&
        r == room &&
        ts == entryTypes &&
        rs == rescheduled &&
        rt == retry &&
        i == id
    case _ => false
  }
}