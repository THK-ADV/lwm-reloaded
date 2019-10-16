package database.helper

import database._
import slick.jdbc.JdbcProfile

trait DatabaseTables {

  implicit val profile: JdbcProfile

  import profile.api._

  private def schemas = List(
    TableQuery[RoleTable].schema,
    TableQuery[DegreeTable].schema,
    TableQuery[UserTable].schema,
    TableQuery[SemesterTable].schema,
    TableQuery[CourseTable].schema,
    TableQuery[AuthorityTable].schema,
    TableQuery[LabworkTable].schema,
    TableQuery[LabworkApplicationTable].schema,
    TableQuery[LabworkApplicationFriendTable].schema,
    TableQuery[RoomTable].schema,
    TableQuery[AssignmentEntryTable].schema,
    TableQuery[AssignmentEntryTypeTable].schema,
    TableQuery[BlacklistTable].schema,
    TableQuery[TimetableTable].schema,
    TableQuery[TimetableBlacklistTable].schema,
    TableQuery[TimetableEntryTable].schema,
    TableQuery[TimetableEntrySupervisorTable].schema,
    TableQuery[ReportCardEntryTable].schema,
    TableQuery[ReportCardRescheduledTable].schema,
    TableQuery[ReportCardRetryTable].schema,
    TableQuery[ReportCardEntryTypeTable].schema,
    TableQuery[ReportCardEvaluationTable].schema,
    TableQuery[ReportCardEvaluationPatternTable].schema,
    TableQuery[GroupTable].schema,
    TableQuery[GroupMembershipTable].schema,
    TableQuery[ScheduleEntryTable].schema,
    TableQuery[ScheduleEntrySupervisorTable].schema
  )

  protected def createAction(): DBIOAction[Unit, NoStream, Effect.Schema] = DBIO.seq(schemas.map(_.create): _*)

  protected def dropAction(): DBIOAction[Unit, NoStream, Effect.Schema] = DBIO.seq(schemas.reverseMap(_.drop): _*)
}
