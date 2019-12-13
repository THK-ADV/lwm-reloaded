package database.helper

import database._
import slick.jdbc.JdbcProfile

trait DatabaseTables {

  implicit val profile: JdbcProfile

  import profile.api._

  protected def tables = List(
    TableQuery[RoleTable],
    TableQuery[DegreeTable],
    TableQuery[UserTable],
    TableQuery[SemesterTable],
    TableQuery[CourseTable],
    TableQuery[AuthorityTable],
    TableQuery[LabworkTable],
    TableQuery[LabworkApplicationTable],
    TableQuery[LabworkApplicationFriendTable],
    TableQuery[RoomTable],
    TableQuery[AssignmentEntryTable],
    TableQuery[AssignmentTypeTable],
    TableQuery[BlacklistTable],
    TableQuery[TimetableTable],
    TableQuery[TimetableBlacklistTable],
    TableQuery[TimetableEntryTable],
    TableQuery[TimetableEntrySupervisorTable],
    TableQuery[ReportCardEntryTable],
    TableQuery[ReportCardRescheduledTable],
    TableQuery[ReportCardRetryTable],
    TableQuery[ReportCardEntryTypeTable],
    TableQuery[ReportCardEvaluationTable],
    TableQuery[ReportCardEvaluationPatternTable],
    TableQuery[GroupTable],
    TableQuery[GroupMembershipTable],
    TableQuery[ScheduleEntryTable],
    TableQuery[ScheduleEntrySupervisorTable]
  )

  protected def createAction(): DBIOAction[Unit, NoStream, Effect.Schema] = DBIO.seq(tables.map(_.schema.create): _*)

  protected def dropAction(): DBIOAction[Unit, NoStream, Effect.Schema] = DBIO.seq(tables.reverseMap(_.schema.drop): _*)
}
