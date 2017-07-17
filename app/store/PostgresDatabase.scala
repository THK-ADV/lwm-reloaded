package store

import java.util.UUID

import models._
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep
import java.sql.{Date, Time, Timestamp}

trait UniqueTable { self: Table[_] =>
  def id = column[UUID]("ID", O.PrimaryKey)
  def invalidated = column[Option[Timestamp]]("INVALIDATED")
  def lastModified = column[Timestamp]("LAST_MODIFIED")

  final def isValid: Rep[Boolean] = invalidated.isEmpty

  final def lastModifiedSince(timestamp: Timestamp): Rep[Boolean] = lastModified >= timestamp
}

trait LabworkIdTable { self: Table[_] =>
  def labwork = column[UUID]("LABWORK")

  def labworkFk = foreignKey("LABWORKS_fkey", labwork, TableQuery[LabworkTable])(_.id)
  def joinLabwork = TableQuery[LabworkTable].filter(_.id === labwork)
}

trait RoomIdTable { self: Table[_] =>
  def room = column[UUID]("ROOM")
  def roomFk = foreignKey("ROOMS_fkey", room, TableQuery[RoomTable])(_.id)
}

trait TimetableIdTable { self: Table[_] =>
  def timetable = column[UUID]("TIMETABLE")
  def timetableFk = foreignKey("TIMETABLES_fkey", timetable, TableQuery[TimetableTable])(_.id)
}

trait ReportCardEntryIdTable { self: Table[_] =>
  def reportCardEntry = column[UUID]("REPORT_CARD_ENTRY")

  def reportCardEntryFk = foreignKey("REPORT_CARD_ENTRY_fkey", reportCardEntry, TableQuery[ReportCardEntryTable])(_.id)
  def joinReportCardEntry = TableQuery[ReportCardEntryTable].filter(_.id === reportCardEntry)
}

trait LabelTable { self: Table[_] =>
  def label = column[String]("LABEL")
}

trait DescriptionTable { self: Table[_] =>
  def description = column[String]("DESCRIPTION")
}

trait AbbreviationTable { self: Table[_] =>
  def abbreviation = column[String]("ABBREVIATION")
}

trait DateStartEndTable { self: Table[_] =>
  def date = column[Date]("DATE")
  def start = column[Time]("START")
  def end = column[Time]("END")
}

trait GroupIdTable { self: Table[_] =>
  def group = column[UUID]("GROUP")

  def groupFk = foreignKey("GROUP_fkey", group, TableQuery[GroupTable])(_.id)
  def joinGroup = TableQuery[GroupTable].filter(_.id === group)
}

trait StudentIdTable { self: Table[_] =>
  def student = column[UUID]("STUDENT")

  def studentFk = foreignKey("STUDENTS_fkey", student, TableQuery[UserTable])(_.id)
}

trait TableFilter[T <: Table[_]] {
  def value: String

  def predicate: T => Rep[Boolean]
}

class UserTable(tag: Tag) extends Table[DbUser](tag, "USERS") with UniqueTable {
  def systemId = column[String]("SYSTEM_ID")
  def lastname = column[String]("LASTNAME")
  def firstname = column[String]("FIRSTNAME")
  def email = column[String]("EMAIL")
  def registrationId = column[Option[String]]("REGISTRATION_ID")
  def enrollment = column[Option[UUID]]("ENROLLMENT")
  def status = column[String]("STATUS")

  def degreeFk = foreignKey("DEGREES_fkey", enrollment, TableQuery[DegreeTable])(_.id.?)

  //def labworkApplication(labwork: UUID) = TableQuery[LabworkApplicationTable].filter(lapp => lapp.applicant === id && lapp.labwork === labwork)

  override def * = (systemId, lastname, firstname, email, status, registrationId, enrollment, lastModified, invalidated, id) <> ((DbUser.apply _).tupled, DbUser.unapply)
}

class DegreeTable(tag: Tag) extends Table[DegreeDb](tag, "DEGREES") with UniqueTable with LabelTable with AbbreviationTable {

  def labworks =  TableQuery[LabworkTable].filter(_.degree === id)

  override def * = (label, abbreviation, lastModified, invalidated, id) <> ((DegreeDb.apply _).tupled, DegreeDb.unapply)
}

class AuthorityTable(tag: Tag) extends Table[AuthorityDb](tag, "AUTHORITIES") with UniqueTable {
  def user = column[UUID]("USER")
  def role = column[UUID]("ROLE")
  def course = column[Option[UUID]]("COURSE")

  def userFk = foreignKey("USERS_fkey", user, TableQuery[UserTable])(_.id)
  def courseFk = foreignKey("COURSES_fkey", course, TableQuery[CourseTable])(_.id.?)
  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)

  override def * = (user, role, course, lastModified, invalidated, id) <> ((AuthorityDb.apply _).tupled, AuthorityDb.unapply)
}
class SemesterTable(tag: Tag) extends Table[SemesterDb](tag, "SEMESTERS") with UniqueTable with LabelTable with AbbreviationTable {
  def start = column[Date]("START")
  def end = column[Date]("END")
  def examStart = column[Date]("EXAM_START")

  override def * = (label, abbreviation, start, end, examStart, lastModified, invalidated, id) <> ((SemesterDb.apply _).tupled, SemesterDb.unapply)
}

class CourseTable(tag: Tag) extends Table[CourseDb](tag, "COURSES") with UniqueTable with LabelTable with DescriptionTable with AbbreviationTable {
  def lecturer = column[UUID]("LECTURER")
  def semesterIndex = column[Int]("SEMESTER_INDEX")

  def lecturerFk = foreignKey("LECTURERS_fkey", lecturer, TableQuery[UserTable])(_.id)

  def joinLecturer = TableQuery[UserTable].filter(_.id === lecturer)

  override def * = (label, description, abbreviation, lecturer, semesterIndex, lastModified, invalidated, id) <> ((CourseDb.apply _).tupled, CourseDb.unapply)
}

class LabworkTable(tag: Tag) extends Table[LabworkDb](tag, "LABWORK") with UniqueTable with LabelTable with DescriptionTable {
  def semester = column[UUID]("SEMESTER")
  def course = column[UUID]("COURSE")
  def degree = column[UUID]("DEGREE")
  def subscribable = column[Boolean]("SUBSCRIBABLE")
  def published = column[Boolean]("PUBLISHED")

  def semesterFk = foreignKey("SEMESTERS_fkey", semester, TableQuery[SemesterTable])(_.id)
  def courseFk = foreignKey("COURSES_fkey", course, TableQuery[CourseTable])(_.id)
  def degreeFk = foreignKey("DEGREES_fkey", degree, TableQuery[DegreeTable])(_.id)

  def joinCourse = TableQuery[CourseTable].filter(_.id === course)
  def joinDegree = TableQuery[DegreeTable].filter(_.id === degree)
  def joinSemester = TableQuery[SemesterTable].filter(_.id === semester)

  def fullJoin = {
    for {
      c <- joinCourse
      d <- joinDegree
      s <- joinSemester
    } yield (c, d, s)
  }

  override def * = (label, description, semester, course, degree, subscribable, published, lastModified, invalidated, id) <> ((LabworkDb.apply _).tupled, LabworkDb.unapply)
}

class LabworkApplicationTable(tag: Tag) extends Table[LabworkApplicationDb](tag, "LABWORKAPPLICATIONS") with UniqueTable with LabworkIdTable {
  def applicant = column[UUID]("APPLICANT")
  def timestamp = column[Timestamp]("TIMESTAMP")

  override def * = (labwork, applicant, timestamp, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, Timestamp, Timestamp, Option[Timestamp], UUID)) => LabworkApplicationDb = {
    case (labwork, applicant, timestamp, lastModified, invalidated, id) =>
      LabworkApplicationDb(labwork, applicant, Set.empty, timestamp, lastModified, invalidated, id)
  }

  def unmapRow: (LabworkApplicationDb) => Option[(UUID, UUID, Timestamp, Timestamp, Option[Timestamp], UUID)] = { lapp =>
    Option((lapp.labwork, lapp.applicant, lapp.timestamp, lapp.lastModified, lapp.invalidated, lapp.id))
  }

  def friends = TableQuery[LabworkApplicationFriendTable].filter(_.labworkApplication === id).flatMap(_.friendFk)
  def joinApplicant = TableQuery[UserTable].filter(_.id === applicant)

  def fullJoin = {
    for {
      f <- friends
      l <- joinLabwork
      a <- joinApplicant
    } yield (f, a, l)
  }
}

class LabworkApplicationFriendTable(tag: Tag) extends Table[LabworkApplicationFriend](tag, "LABWORKAPPLICATION_FRIEND") with UniqueTable {
  def labworkApplication = column[UUID]("LABWORKAPPLICATION")
  def friend = column[UUID]("FRIEND")

  def labworkApplicationFk = foreignKey("LABWORKAPPLICATIONS_fkey", labworkApplication, TableQuery[LabworkApplicationTable])(_.id)
  def friendFk = foreignKey("USERS_fkey", friend, TableQuery[UserTable])(_.id)

  override def * = (labworkApplication, friend, lastModified, invalidated, id) <> ((LabworkApplicationFriend.apply _).tupled, LabworkApplicationFriend.unapply)
}

class RoleTable(tag: Tag) extends Table[RoleDb](tag, "ROLES") with UniqueTable with LabelTable {
  override def * = (label, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((String, Timestamp, Option[Timestamp], UUID)) => RoleDb = {
    case (label, lastModified, invalidated, id) => RoleDb(label, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: (RoleDb) => Option[(String, Timestamp, Option[Timestamp], UUID)] = { role =>
    Option((role.label, role.lastModified, role.invalidated, role.id))
  }

  def isLabel(label: String): Rep[Boolean] = this.label === label

  def permissions = TableQuery[RolePermissionTable].filter(_.role === id).flatMap(_.permissionFk)
}

class PermissionTable(tag: Tag) extends Table[PermissionDb](tag, "PERMISSIONS") with UniqueTable with DescriptionTable {
  def value = column[String]("VALUE")

  override def * = (value, description, lastModified, invalidated, id) <> ((PermissionDb.apply _).tupled, PermissionDb.unapply)
}

class RolePermissionTable(tag: Tag) extends Table[RolePermission](tag, "ROLE_PERMISSION") with UniqueTable {
  def role = column[UUID]("ROLE")
  def permission = column[UUID]("PERMISSION")

  def roleFk = foreignKey("ROLES_fkey", role, TableQuery[RoleTable])(_.id)
  def permissionFk = foreignKey("PERMISSIONS_fkey", permission, TableQuery[PermissionTable])(_.id)

  override def * = (role, permission, id) <> ((RolePermission.apply _).tupled, RolePermission.unapply)
}

class RoomTable(tag: Tag) extends Table[RoomDb](tag, "ROOMS") with UniqueTable with LabelTable with DescriptionTable {

  override def * = (label, description, lastModified, invalidated, id) <> ((RoomDb.apply _).tupled, RoomDb.unapply)
}

class AssignmentPlanTable(tag: Tag) extends Table[AssignmentPlanDb](tag, "ASSIGNMENT_PLAN") with UniqueTable with LabworkIdTable {
  def attendance = column[Int]("ATTENDANCE")
  def mandatory = column[Int]("MANDATORY")

  override def * = (labwork, attendance, mandatory, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Int, Int, Timestamp, Option[Timestamp], UUID)) => AssignmentPlanDb = {
    case (labwork, attendance, mandatory, lastModified, invalidated, id) =>
      AssignmentPlanDb(labwork, attendance, mandatory, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: (AssignmentPlanDb) => Option[(UUID, Int, Int, Timestamp, Option[Timestamp], UUID)] = { plan =>
    Option((plan.labwork, plan.attendance, plan.mandatory, plan.lastModified, plan.invalidated, plan.id))
  }
}

class AssignmentEntryTable(tag: Tag) extends Table[AssignmentEntryDb](tag, "ASSIGNMENT_ENTRY") with UniqueTable with LabelTable {
  def assignmentPlan = column[UUID]("ASSIGNMENT_PLAN")
  def index = column[Int]("INDEX")
  def duration = column[Int]("DURATION")

  def assignmentPlanFk = foreignKey("ASSIGNMENT_PLANS_fkey", assignmentPlan, TableQuery[AssignmentPlanTable])(_.id)

  override def * = (assignmentPlan, index, label, duration, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Int, String, Int, UUID)) => AssignmentEntryDb = {
    case (assignmentPlan, index, label, duration, id) => AssignmentEntryDb(assignmentPlan, index, label, Set.empty, duration, id)
  }

  def unmapRow: (AssignmentEntryDb) => Option[(UUID, Int, String, Int, UUID)] = { entry =>
    Option((entry.assignmentPlan, entry.index, entry.label, entry.duration, entry.id))
  }
}

class AssignmentEntryTypeTable(tag: Tag) extends Table[AssignmentEntryTypeDb](tag, "ASSIGNMENT_ENTRY_TYPE") with UniqueTable {
  def assignmentEntry = column[UUID]("ASSIGNMENT_ENTRY")
  def entryType = column[String]("ENTRY_TYPE")
  def bool = column[Boolean]("BOOL")
  def int = column[Int]("INT")

  def assignmentEntryFk = foreignKey("ASSIGNMENT_ENTRIES_fkey", assignmentEntry, TableQuery[AssignmentEntryTable])(_.id)

  override def * = (assignmentEntry, entryType, bool, int, id) <> ((AssignmentEntryTypeDb.apply _).tupled, AssignmentEntryTypeDb.unapply)
}

class BlacklistTable(tag: Tag) extends Table[BlacklistDb](tag, "BLACKLIST") with UniqueTable with LabelTable with DateStartEndTable {
  def global = column[Boolean]("GLOBAL")

  override def * = (label, date, start, end, global, lastModified, invalidated, id) <> ((BlacklistDb.apply _).tupled, BlacklistDb.unapply)
}

class TimetableTable(tag: Tag) extends Table[TimetableDb](tag, "TIMETABLE") with UniqueTable with LabworkIdTable {
  def start = column[Date]("START")

  override def * = (labwork, start, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Date, Timestamp, Option[Timestamp], UUID)) => TimetableDb = {
    case (labwork, start, lastModified, invalidated, id) =>
      TimetableDb(labwork, Set.empty, start, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: (TimetableDb) => Option[(UUID, Date, Timestamp, Option[Timestamp], UUID)] = { timetable =>
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

  def unmapRow: (TimetableEntryDb) => Option[(UUID, UUID, Int, Time, Time, UUID)] = { entry =>
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

class ReportCardEntryTable(tag: Tag) extends Table[ReportCardEntryDb](tag, "REPORT_CARD_ENTRY") with UniqueTable with LabworkIdTable with LabelTable with DateStartEndTable with RoomIdTable with StudentIdTable {

  override def * = (student, labwork, label, date, start, end, room, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, UUID, String, Date, Time, Time, UUID, Timestamp, Option[Timestamp], UUID)) => ReportCardEntryDb = {
    case (student, labwork, label, date, start, end, room, lastModified, invalidated, id) =>
      ReportCardEntryDb(student, labwork, label, date, start, end, room, Set.empty, None, None, lastModified, invalidated, id)
  }

  def unmapRow: (ReportCardEntryDb) => Option[(UUID, UUID, String, Date, Time, Time, UUID, Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.student, entry.labwork, entry.label, entry.date, entry.start, entry.end, entry.room, entry.lastModified, entry.invalidated, entry.id))
  }
}

class ReportCardRetryTable(tag: Tag) extends Table[ReportCardRetryDb](tag, "REPORT_CARD_RETRY") with UniqueTable with DateStartEndTable with RoomIdTable with ReportCardEntryIdTable {
  def reason = column[Option[String]]("REASON")

  override def * = (reportCardEntry, date, start, end, room, reason, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Date, Time, Time, UUID, Option[String], Timestamp, Option[Timestamp], UUID)) => ReportCardRetryDb = {
    case (reportCardEntry, date, start, end, room, reason, lastModified, invalidated, id) =>
      ReportCardRetryDb(reportCardEntry, date, start, end, room, Set.empty, reason, lastModified, invalidated, id)
  }

  def unmapRow: (ReportCardRetryDb) => Option[(UUID, Date, Time, Time, UUID, Option[String], Timestamp, Option[Timestamp], UUID)] = { entry =>
    Option((entry.reportCardEntry, entry.date, entry.start, entry.end, entry.room, entry.reason, entry.lastModified, entry.invalidated, entry.id))
  }
}

class ReportCardRescheduledTable(tag: Tag) extends Table[ReportCardRescheduledDb](tag, "REPORT_CARD_RESCHEDULED") with UniqueTable with DateStartEndTable with RoomIdTable with ReportCardEntryIdTable {
  def reason = column[Option[String]]("REASON")

  override def * = (reportCardEntry, date, start, end, room, reason, lastModified, invalidated, id) <> ((ReportCardRescheduledDb.apply _).tupled, ReportCardRescheduledDb.unapply)
}

class ReportCardEntryTypeTable(tag: Tag) extends Table[ReportCardEntryTypeDb](tag, "REPORT_CARD_ENTRY_TYPE") with UniqueTable {
  def entryType = column[String]("ENTRY_TYPE")
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

class ScheduleEntryTable(tag: Tag) extends Table[ScheduleEntryDb](tag, "SCHEDULE_ENTRY") with UniqueTable with LabworkIdTable with RoomIdTable with GroupIdTable with DateStartEndTable {

  override def * = (labwork, start, end, date, room, group, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((UUID, Time, Time, Date, UUID, UUID, Timestamp, Option[Timestamp], UUID)) => ScheduleEntryDb = {
    case (labwork, start, end, date, room, group, lastModified, invalidated, id) =>
      ScheduleEntryDb(labwork, start, end, date, room, Set.empty, group, lastModified, invalidated, id)
  }

  def unmapRow: (ScheduleEntryDb) => Option[(UUID, Time, Time, Date, UUID, UUID, Timestamp, Option[Timestamp], UUID)] = { entry =>
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

class GroupTable(tag: Tag) extends Table[GroupDb](tag, "GROUP") with UniqueTable with LabworkIdTable with LabelTable {

  override def * = (label, labwork, lastModified, invalidated, id) <> (mapRow, unmapRow)

  def mapRow: ((String, UUID, Timestamp, Option[Timestamp], UUID)) => GroupDb = {
    case (label, labwork, lastModified, invalidated, id) => GroupDb(label, labwork, Set.empty, lastModified, invalidated, id)
  }

  def unmapRow: (GroupDb) => Option[(String, UUID, Timestamp, Option[Timestamp], UUID)] = { group =>
    Option((group.label, group.labwork, group.lastModified, group.invalidated, group.id))
  }
}

class GroupMembershipTable(tag: Tag) extends Table[GroupMembership](tag, "GROUP_MEMBERSHIP") with UniqueTable with GroupIdTable with StudentIdTable {
  override def * = (group, student, id) <> ((GroupMembership.apply _).tupled, GroupMembership.unapply)
}