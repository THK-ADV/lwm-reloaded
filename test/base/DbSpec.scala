package base

import models.{RoleDb, Roles}
import org.scalatest._
import org.scalatest.concurrent.{Futures, ScalaFutures}
import org.w3.banana.sesame.{Sesame, SesameModule}
import org.w3.banana.{RDF, RDFModule}
import store.Prefixes.LWMPrefix
import store.bind.Bindings
import store._

import scala.concurrent.{Await, Future}

abstract class PostgresDbSpec extends WordSpec with TestBaseDefinition {
  import slick.driver.PostgresDriver.api._
  import scala.concurrent.duration._

  lazy val db = Database.forConfig("database_test")
  implicit lazy val executionContext = scala.concurrent.ExecutionContext.Implicits.global

  protected final def await[R](future: Future[R]): R = Await.result(future, Duration.Inf)
  protected final def run[R](action: DBIOAction[R, NoStream, Nothing]): R = await(db.run(action))

  protected def dependencies: DBIOAction[Unit, NoStream, Effect.Write]

  private val schema = List(
    TableQuery[PermissionTable].schema,
    TableQuery[RoleTable].schema,
    TableQuery[RolePermissionTable].schema,
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
    TableQuery[TimetableEntrySupervisorTable].schema
  )

  private val mandatoryFill = DBIO.seq(
    TableQuery[RoleTable].forceInsertAll(Roles.all.map(l => RoleDb(l, Set.empty)))
  )

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    await(db.run(DBIO.seq(
      schema.map(_.create): _*
    ).andThen(dependencies).
      andThen(mandatoryFill).
      transactionally)
    )
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    await(db.run(DBIO.seq(
      schema.reverseMap(_.drop): _*
    ).transactionally))
  }
}

abstract class SesameDbSpec extends DbSpec[Sesame] with SesameModule {
  val lwm = LWMPrefix[Sesame]
  implicit val namespace = Namespace(s"http://testDB/${this.getClass.getSimpleName}")

  val bindings = Bindings[Sesame](namespace)

  lazy val repo = SesameRepository(namespace)

  override def initDB(): Unit = {}

  override def prepareDB(): Unit = {
    initDB()
  }

  override def destroyDB(): Unit = {
    repo.close()
  }
}


trait DbSpec[R <: RDF] extends WordSpec with RDFModule with TestBaseDefinition {
  def repo: SemanticRepository

  def initDB(): Unit

  def prepareDB(): Unit

  def destroyDB(): Unit

  override protected def beforeAll(): Unit = {
    super.beforeAll()
    prepareDB()
  }

  override protected def afterAll(): Unit = {
    super.afterAll()
    destroyDB()
  }
}

trait TestBaseDefinition extends BeforeAndAfterAll with BeforeAndAfterEach with Matchers with ScalaFutures with OptionValues with EitherValues {
  this: Suite =>
}
