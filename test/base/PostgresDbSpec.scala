package base

import database._

abstract class PostgresDbSpec extends DatabaseSpec {

  import profile.api._

  protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write]

  private val schema = List(
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
    TableQuery[AssignmentPlanTable].schema,
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

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    createSchemas()
  }

  protected def createSchemas(): Unit = runAsyncSequence(schema.map(_.create) :+ dependencies: _*)

  override protected def afterAll(): Unit = {
    super.afterAll()

    dropSchemas()
    db.close()
  }

  protected def dropSchemas(): Unit = runAsyncSequence(schema.reverseMap(_.drop): _*)
}
