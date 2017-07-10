package services

import java.sql.{Date, Timestamp}
import java.util.UUID

import base.PostgresDbSpec
import models._
import org.joda.time.LocalDate
import slick.dbio.Effect.Write
import store.{RoleTable, UniqueTable}
import slick.driver.PostgresDriver.api._

object AbstractDaoSpec {
  import scala.util.Random.{nextInt, nextBoolean}

  val maxDegrees = 10
  val maxLabworks = 20
  val maxSemesters = 10
  val maxCourses = 10
  val maxRooms = 10
  val maxEmployees = 10
  val maxAssignmentPlans = 10
  val maxPermissions = 10
  val maxAuthorities = 10

  def randomSemester = semesters(nextInt(maxSemesters))
  def randomCourse = courses(nextInt(maxCourses))
  def randomDegree = degrees(nextInt(maxDegrees))
  def randomLabwork = labworks(nextInt(maxDegrees))
  def randomRoom = rooms(nextInt(maxRooms))
  def randomEmployee = employees(nextInt(maxEmployees))
  def randomPermission = permissions(nextInt(maxPermissions))
  def randomRole = roles(nextInt(roles.length))
  def randomAuthority = authorities(nextInt(maxAuthorities))

  val semesters = {
    val template = LocalDate.now.withDayOfWeek(1).withMonthOfYear(9).minusYears(5).plusMonths(6)

    (0 until maxSemesters).foldLeft((List.empty[SemesterDb], template)) {
      case ((list, t), i) =>
        val start = new Date(t.plusDays(1).toDateTimeAtStartOfDay.getMillis)
        val end = t.plusDays(1).plusMonths(6)
        val exam = new Date(t.plusDays(1).plusMonths(5).toDateTimeAtStartOfDay.getMillis)

        val current = SemesterDb(i.toString, i.toString, start, new Date(end.toDateTimeAtStartOfDay.getMillis), exam)
        (list.:+(current), end)
    }._1
  }

  val employees = (0 until maxEmployees).map(i => DbUser(i.toString, i.toString, i.toString, i.toString, User.EmployeeType, None, None)).toList

  val courses = (0 until maxCourses).map { i =>
    CourseDb(i.toString, i.toString, i.toString, randomEmployee.id, i % 6)
  }.toList

  val authorities = (0 until maxAuthorities).map{ i =>
    val role:RoleDb = roles((i*3)%roles.length)
    val course:Option[UUID] = if(role.label == Roles.RightsManagerLabel) Some(courses((i*6)%maxCourses).id) else None
    AuthorityDb(employees(i%maxEmployees).id, role.id, course)
  }.toList

  lazy val roles = Roles.all.map(l => RoleDb(l, Set.empty))

  val degrees = (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList

  val labworks = (0 until maxLabworks).map { i =>
    LabworkDb(i.toString, i.toString, randomSemester.id, randomCourse.id, randomDegree.id)
  }.toList

  val rooms = (0 until maxRooms).map(i => RoomDb(i.toString, i.toString)).toList

  val assignmentPlans = (0 until maxAssignmentPlans).map { i =>
    val entries = (0 until 10).map { j =>
      val allTypes = PostgresAssignmentEntryType.all
      PostgresAssignmentEntry(j, j.toString, allTypes.take(nextInt(allTypes.size - 1) + 1))
    }

    AssignmentPlanDb(labworks(i).id, i, i, entries.toSet)
  }.toList

  val permissions = (0 until maxPermissions).map{ i =>
    PermissionDb(s"label$i", s"description$i")
  }.toList

}

abstract class AbstractDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity]
  extends PostgresDbSpec with AbstractDao[T, DbModel, LwmModel] {

  protected val lastModified: Timestamp = {
    import models.LwmDateTime.DateTimeConverter
    import org.joda.time.DateTime

    DateTime.now.timestamp
  }

  protected def name: String
  protected def dbEntity: DbModel // dbEntity should not expand
  protected def invalidDuplicateOfDbEntity: DbModel // invalidDuplicateOfDbEntity should not expand
  protected def invalidUpdateOfDbEntity: DbModel // invalidUpdateOfDbEntity should not expand
  protected def validUpdateOnDbEntity: DbModel // validUpdateOnDbEntity should not expand
  protected def dbEntities: List[DbModel] // dbEntities should not expand

  protected def lwmEntity: LwmModel
  protected def lwmAtom: LwmModel

  override protected def dependencies: DBIOAction[Unit, NoStream, Write]

  s"A AbstractDaoSpec with $name " should {

    s"create a $name" in {
      await(create(dbEntity)) shouldBe dbEntity
    }

    s"get a $name" in {
      await(getById(dbEntity.id.toString, atomic = false)) shouldBe Some(lwmEntity)
      await(getById(dbEntity.id.toString)) shouldBe Some(lwmAtom)
    }

    s"not create a $name because model already exists" in {
      await(create(invalidDuplicateOfDbEntity).failed) shouldBe ModelAlreadyExists(Seq(dbEntity))
    }

    s"not update a $name because model already exists" in {
      await(update(invalidUpdateOfDbEntity).failed) shouldBe ModelAlreadyExists(dbEntity)
    }

    s"update a $name properly" in {
      await(update(validUpdateOnDbEntity)) shouldBe Some(validUpdateOnDbEntity)
    }

    s"create many $name" in {
      await(createMany(dbEntities)) shouldBe dbEntities
    }

    s"delete a $name by invalidating it" in {
      await(delete(dbEntity)) shouldBe defined
    }
  }
}
