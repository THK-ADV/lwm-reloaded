package store

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models.{ScheduleEntry, UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

class ScheduleEntryTable(tag: Tag) extends Table[ScheduleEntryDb](tag, "SCHEDULE_ENTRY") with UniqueTable with LabworkIdTable with RoomIdTable with GroupIdTable with DateStartEndTable {

  override def * = (labwork, start, end, date, room, group, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Time, Time, Date, UUID, UUID, Timestamp, Option[Timestamp], UUID)) => ScheduleEntryDb = {
    case (labwork, start, end, date, room, group, lastModified, invalidated, id) =>
      ScheduleEntryDb(labwork, start, end, date, room, Set.empty, group, lastModified, invalidated, id)
  }

  def unmapRow: ScheduleEntryDb => Option[(UUID, Time, Time, Date, UUID, UUID, Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.labwork, entry.start, entry.end, entry.date, entry.room, entry.group, entry.lastModified, entry.invalidated, entry.id))
  }
}

class ScheduleEntrySupervisorTable(tag: Tag) extends Table[ScheduleEntrySupervisor](tag, "SCHEDULE_ENTRY_SUPERVISOR") with UniqueTable {
  def scheduleEntry = column[UUID]("SCHEDULE_ENTRY")

  def supervisor = column[UUID]("SUPERVISOR")

  def scheduleEntryFk = foreignKey("SCHEDULE_ENTRIES_fkey", scheduleEntry, TableQuery[ScheduleEntryTable])(_.id)

  def supervisorFk = foreignKey("USERS_fkey", supervisor, TableQuery[UserTable])(_.id)

  def joinSupervisor = TableQuery[UserTable].filter(_.id === supervisor)

  override def * = (scheduleEntry, supervisor, id) <> ((ScheduleEntrySupervisor.apply _).tupled, ScheduleEntrySupervisor.unapply)
}

case class ScheduleEntryDb(labwork: UUID, start: Time, end: Time, date: Date, room: UUID, supervisor: Set[UUID], group: UUID, lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = ScheduleEntry(labwork, start.localTime, end.localTime, date.localDate, room, supervisor, group, id)

  override def equals(that: scala.Any) = that match {
    case ScheduleEntryDb(l, s, e, d, r, sup, g, _, _, i) =>
      l == labwork &&
        s.localTime.isEqual(start.localTime) &&
        e.localTime.isEqual(end.localTime) &&
        d.localDate.isEqual(date.localDate) &&
        r == room &&
        sup == supervisor &&
        g == group &&
        i == id
    case _ => false
  }
}

case class ScheduleEntrySupervisor(scheduleEntry: UUID, supervisor: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity
