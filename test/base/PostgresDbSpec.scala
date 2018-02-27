package base

import modules.DatabaseModule
import org.scalatest._
import store._

import scala.concurrent.{Await, ExecutionContextExecutor, Future}

object PostgresDbSpec {
  import slick.driver.PostgresDriver.api._

  lazy val db = Database.forConfig("database_test")
}

abstract class PostgresDbSpec extends WordSpec with TestBaseDefinition with DatabaseModule {
  import slick.driver.PostgresDriver.api._
  import scala.concurrent.duration._

  override def db = base.PostgresDbSpec.db

  implicit lazy val executionContext: ExecutionContextExecutor = scala.concurrent.ExecutionContext.Implicits.global

  protected final def await[R](future: Future[R]): R = Await.result(future, Duration.Inf)
  protected final def run[R](action: DBIOAction[R, NoStream, Nothing]): R = await(db.run(action))

  protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write]

  private val tableQueries = List(
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
    TableQuery[AssignmentPlanTable],
    TableQuery[AssignmentEntryTable],
    TableQuery[AssignmentEntryTypeTable],
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
    TableQuery[GroupTable],
    TableQuery[GroupMembershipTable],
    TableQuery[ScheduleEntryTable],
    TableQuery[ScheduleEntrySupervisorTable]
  )

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    await(db.run(DBIO.seq(tableQueries.map(_.schema.create): _*).andThen(dependencies).transactionally))
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    await(db.run(DBIO.seq(tableQueries.reverseMap(_.schema.drop): _*).transactionally))
  }

  protected def clear(): Unit = await(db.run(DBIO.seq(tableQueries.reverseMap(_.delete): _*)))
}
