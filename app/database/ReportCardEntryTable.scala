package database

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models._
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

class ReportCardEntryTable(tag: Tag) extends Table[ReportCardEntryDb](tag, "REPORT_CARD_ENTRY") with UniqueTable with LabworkIdTable with LabelTable with DateStartEndTable with RoomIdTable with UserIdTable {
  override protected def userColumnName: String = "STUDENT"

  def assignmentIndex = column[Int]("ASSIGNMENT_INDEX")

  override def * = (user, labwork, label, date, start, end, room, assignmentIndex, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def joinAssignmentEntry(id: UUID) = // TODO replace with real join is ReportCardEntryType has an assignmentEntryFk
    TableQuery[AssignmentEntryTable].filter(a => a.isValid && a.id === id && a.label === label && a.index === assignmentIndex)

  def mapRow: ((UUID, UUID, String, Date, Time, Time, UUID, Int, Timestamp, Option[Timestamp], UUID)) => ReportCardEntryDb = {
    case (student, labwork, label, date, start, end, room, index, lastModified, invalidated, id) =>
      ReportCardEntryDb(student, labwork, label, date, start, end, room, Set.empty, index, None, None, lastModified, invalidated, id)
  }

  def unmapRow: ReportCardEntryDb => Option[(UUID, UUID, String, Date, Time, Time, UUID, Int, Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.assignmentIndex, entry.lastModified, entry.invalidated, entry.id))
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
  assignmentIndex: Int = -1,
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
    assignmentIndex,
    rescheduled.map(_.toUniqueEntity),
    retry.map(_.toUniqueEntity),
    id
  )

  override def equals(that: scala.Any) = that match {
    case ReportCardEntryDb(s, l, lb, dt, st, et, r, ts, idx, rs, rt, _, _, i) =>
      s == student &&
        l == labwork &&
        lb == label &&
        dt.localDate == date.localDate &&
        st.localTime == start.localTime &&
        et.localTime == end.localTime &&
        r == room &&
        idx == assignmentIndex &&
        ts == entryTypes &&
        rs == rescheduled &&
        rt == retry &&
        i == id
    case _ => false
  }
}