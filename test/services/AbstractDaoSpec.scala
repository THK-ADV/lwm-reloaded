package services

import java.sql.{Date, Timestamp}
import models.LwmDateTime._

import base.PostgresDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}
import slick.dbio.Effect.Write
import store.UniqueTable
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
  val maxBlacklists = 20
  val maxTimetables = 10

  def randomSemester = semesters(nextInt(maxSemesters))
  def randomCourse = courses(nextInt(maxCourses))
  def randomDegree = degrees(nextInt(maxDegrees))
  def randomLabwork = labworks(nextInt(maxDegrees))
  def randomRoom = rooms(nextInt(maxRooms))
  def randomEmployee = employees(nextInt(maxEmployees))

  def takeSomeOf[A](traversable: Traversable[A]) = {
    traversable.take(nextInt(traversable.size - 1) + 1)
  }

  def populateBlacklists(amount: Int) = (0 until amount).map { i =>
    val date = LocalDate.now.plusDays(i)
    val start = LocalTime.now.plusHours(i)
    val end = start.plusHours(1)

    BlacklistDb(i.toString, date.sqlDate, start.sqlTime, end.sqlTime, nextBoolean)
  }.toList

  def populateLabworks(amount: Int) = (0 until amount).map { i =>
    LabworkDb(i.toString, i.toString, randomSemester.id, randomCourse.id, randomDegree.id)
  }.toList

  def populateEmployees(amount: Int) = (0 until amount).map { i =>
    DbUser(i.toString, i.toString, i.toString, i.toString, User.EmployeeType, None, None)
  }.toList

  def populateTimetables(amount: Int, numberOfEntries: Int)(users: List[DbUser], labworks: List[LabworkDb], blacklists: List[BlacklistDb]) = (0 until amount).map { i =>
    val entries = (0 until numberOfEntries).map { j =>
      PostgresTimetableEntry(takeSomeOf(users).map(_.id).toSet, randomRoom.id, nextInt(5), LocalTime.now.plusHours(j), LocalTime.now.plusHours(j + 1))
    }

    TimetableDb(labworks(i).id, entries.toSet, LocalDate.now.plusDays(i).sqlDate, takeSomeOf(blacklists).map(_.id).toSet)
  }.toList

  val semesters = {
    val template = LocalDate.now.withDayOfWeek(1).withMonthOfYear(9).minusYears(5).plusMonths(6)

    (0 until maxSemesters).foldLeft((List.empty[SemesterDb], template)) {
      case ((list, t), i) =>
        val start = t.plusDays(1)
        val end = start.plusMonths(6)
        val exam = end.minusMonths(1)

        val current = SemesterDb(i.toString, i.toString, start.sqlDate, end.sqlDate, exam.sqlDate)
        (list.:+(current), end)
    }._1
  }

  val employees = populateEmployees(maxEmployees)

  val courses = (0 until maxCourses).map { i =>
    CourseDb(i.toString, i.toString, i.toString, randomEmployee.id, 1)
  }.toList

  val degrees = (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList

  val labworks = populateLabworks(maxLabworks)

  val rooms = (0 until maxRooms).map(i => RoomDb(i.toString, i.toString)).toList

  val assignmentPlans = (0 until maxAssignmentPlans).map { i =>
    val entries = (0 until 10).map { j =>
      val allTypes = PostgresAssignmentEntryType.all
      PostgresAssignmentEntry(j, j.toString, takeSomeOf(allTypes).toSet)
    }

    AssignmentPlanDb(labworks(i).id, i, i, entries.toSet)
  }.toList

  val blacklists = populateBlacklists(maxBlacklists)

  val timetables = populateTimetables(maxTimetables, 6)(employees, labworks.drop(1), blacklists)
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
