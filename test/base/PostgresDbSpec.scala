package base

import database._
import org.scalatest._
import org.scalatest.time.{Seconds, Span}
import org.scalatestplus.play.guice.GuiceOneAppPerSuite
import slick.jdbc.JdbcProfile
import slick.jdbc.JdbcBackend.Database

import scala.concurrent.Future

abstract class PostgresDbSpec extends WordSpec with TestBaseDefinition with GuiceOneAppPerSuite with LwmFakeApplication {
  val db = app.injector.instanceOf(classOf[Database])

  val profile: JdbcProfile = _root_.slick.jdbc.PostgresProfile
  import profile.api._

  protected final def async[R](future: Future[R])(assert: R => Unit): Unit = whenReady(future, timeout(Span(5, Seconds)))(assert)

  protected final def runAsync[R](action: DBIOAction[R, NoStream, Nothing])(assert: R => Unit): Unit = async(db.run(action))(assert)

  protected final def runAsyncSequence[R](args: DBIO[R]*): Unit = async(db.run(DBIO.seq(args: _*).transactionally))(_ => Unit)

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

    runAsyncSequence(schema.map(_.create) :+ dependencies: _*)
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    runAsyncSequence(schema.reverseMap(_.drop): _*)
    db.close()
  }
}
