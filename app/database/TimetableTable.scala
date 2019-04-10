package database

import java.sql.{Date, Time, Timestamp}
import java.util.UUID

import models.{Timetable, TimetableEntry, TimetableProtocol, UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

class TimetableTable(tag: Tag) extends Table[TimetableDb](tag, "TIMETABLE") with UniqueTable with LabworkIdTable {
  def start = column[Date]("START")

  override def * = (labwork, start, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Date, Timestamp, Option[Timestamp], UUID)) => TimetableDb = {
    case (labwork, start, lastModified, invalidated, id) =>
      TimetableDb(labwork, Set.empty, start, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: TimetableDb => Option[(UUID, Date, Timestamp, Option[Timestamp], UUID)] = { timetable =>
    Option((timetable.labwork, timetable.start, timetable.lastModified, timetable.invalidated, timetable.id))
  }
}

class TimetableBlacklistTable(tag: Tag) extends Table[TimetableBlacklist](tag, "TIMETABLE_BLACKLIST") with UniqueTable with TimetableIdTable {
  def blacklist = column[UUID]("BLACKLIST")

  def blacklistFk = foreignKey("BLACKLISTS_fkey", blacklist, TableQuery[BlacklistTable])(_.id)

  override def * = (timetable, blacklist, id) <> ((TimetableBlacklist.apply _).tupled, TimetableBlacklist.unapply)
}

class TimetableEntryTable(tag: Tag) extends Table[TimetableEntryDb](tag, "TIMETABLE_ENTRY") with UniqueTable with TimetableIdTable with RoomIdTable with DateStartEndTable {
  def dayIndex = column[Int]("DAY_INDEX")

  override def * = (timetable, room, dayIndex, start, end, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, Int, Time, Time, UUID)) => TimetableEntryDb = {
    case (timetable, room, dayIndex, start, end, id) =>
      TimetableEntryDb(timetable, room, Set.empty, dayIndex, start, end, id)
  }

  def unmapRow: TimetableEntryDb => Option[(UUID, UUID, Int, Time, Time, UUID)] = { entry =>
    Option((entry.timetable, entry.room, entry.dayIndex, entry.start, entry.end, entry.id))
  }
}

class TimetableEntrySupervisorTable(tag: Tag) extends Table[TimetableEntrySupervisor](tag, "TIMETABLE_ENTRY_SUPERVISOR") with UniqueTable {
  def timetableEntry = column[UUID]("TIMETABLE_ENTRY")

  def supervisor = column[UUID]("SUPERVISOR")

  def timetableEntryFk = foreignKey("TIMETABLE_ENTRIES_fkey", timetableEntry, TableQuery[TimetableEntryTable])(_.id)

  def supervisorFk = foreignKey("USERS_fkey", supervisor, TableQuery[UserTable])(_.id)

  override def * = (timetableEntry, supervisor, id) <> ((TimetableEntrySupervisor.apply _).tupled, TimetableEntrySupervisor.unapply)
}

case class TimetableDb(labwork: UUID, entries: Set[TimetableEntry], start: Date, localBlacklist: Set[UUID], lastModified: Timestamp = DateTime.now.timestamp, invalidated: Option[Timestamp] = None, id: UUID = UUID.randomUUID) extends UniqueDbEntity {
  override def toUniqueEntity = Timetable(labwork, entries, start.localDate, localBlacklist, id)
}

case class TimetableEntryDb(timetable: UUID, room: UUID, supervisor: Set[UUID], dayIndex: Int, start: Time, end: Time, id: UUID = UUID.randomUUID) extends UniqueEntity {
  def toTimetableEntry = TimetableEntry(supervisor, room, dayIndex, start.localTime, end.localTime)
}

case class TimetableEntrySupervisor(timetableEntry: UUID, supervisor: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity

case class TimetableBlacklist(timetable: UUID, blacklist: UUID, id: UUID = UUID.randomUUID) extends UniqueEntity

object TimetableDb {
  def from(p: TimetableProtocol, existing: Option[UUID]) = {
    TimetableDb(p.labwork, p.entries, p.start.sqlDate, p.localBlacklist, id = existing getOrElse UUID.randomUUID)
  }
}
