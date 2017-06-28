package services

import java.sql.{Date, Timestamp}
import java.util.UUID

import models.LwmDateTime._
import base.PostgresDbSpec
import models._
import org.joda.time.{LocalDate, LocalTime}
import slick.dbio.Effect.Write
import store.UniqueTable
import slick.driver.PostgresDriver.api._

object AbstractDaoSpec {
  import scala.util.Random.{nextInt, nextBoolean, shuffle}

  lazy val maxDegrees = 10
  lazy val maxLabworks = 20
  lazy val maxSemesters = 10
  lazy val maxCourses = 10
  lazy val maxRooms = 10
  lazy val maxEmployees = 10
  lazy val maxAssignmentPlans = 10
  lazy val maxBlacklists = 30
  lazy val maxTimetables = 10
  lazy val maxStudents = 100
  lazy val maxReportCardEntries = 100

  def randomSemester = semesters(nextInt(maxSemesters))
  def randomCourse = courses(nextInt(maxCourses))
  def randomDegree = degrees(nextInt(maxDegrees))
  def randomLabwork = labworks(nextInt(maxDegrees))
  def randomRoom = rooms(nextInt(maxRooms))
  def randomEmployee = employees(nextInt(maxEmployees))
  def randomBlacklist = blacklists(nextInt(maxBlacklists))
  def randomStudent = students(nextInt(maxStudents))

  final def takeSomeOf[A](traversable: Traversable[A]) = traversable.take(nextInt(traversable.size - 1) + 1)

  final def takeOneOf[A](traversable: Traversable[A]) = shuffle(traversable).head

  final def populateBlacklists(amount: Int) = (0 until amount).map { i =>
    val global = nextBoolean

    val (date, start, end) = {
      val date = LocalDate.now.plusDays(i)

      if (global) {
        (date, PostgresBlacklist.startOfDay, PostgresBlacklist.endOfDay)
      } else {
        val start = LocalTime.now.withHourOfDay(nextInt(23))
        val end = start.plusHours(1)
        (date, start, end)
      }
    }

    BlacklistDb(i.toString, date.sqlDate, start.sqlTime, end.sqlTime, global)

  }.toList

  final def populateLabworks(amount: Int) = (0 until amount).map { i =>
    LabworkDb(i.toString, i.toString, randomSemester.id, randomCourse.id, randomDegree.id)
  }.toList

  final def populateEmployees(amount: Int) = (0 until amount).map { i =>
    DbUser(i.toString, i.toString, i.toString, i.toString, User.EmployeeType, None, None)
  }.toList

  final def populateStudents(amount: Int) = (0 until amount).map { i =>
    DbUser(i.toString, i.toString, i.toString, i.toString, User.StudentType, Some(i.toString), Some(randomDegree.id))
  }.toList

  final def populateTimetables(amount: Int, numberOfEntries: Int)(users: List[DbUser], labworks: List[LabworkDb], blacklists: List[BlacklistDb]) = (0 until amount).map { i =>
    val entries = (0 until numberOfEntries).map { j =>
      PostgresTimetableEntry(takeSomeOf(users).map(_.id).toSet, randomRoom.id, nextInt(5), LocalTime.now.plusHours(j), LocalTime.now.plusHours(j + 1))
    }

    TimetableDb(labworks(i).id, entries.toSet, LocalDate.now.plusDays(i).sqlDate, takeSomeOf(blacklists).map(_.id).toSet)
  }.toList

  def populateReportCardEntries(amount: Int, numberOfEntries: Int, withRescheduledAndRetry: Boolean)(labworks: List[LabworkDb], students: List[DbUser]) = (0 until amount).flatMap { _ =>
    def reportCardRescheduled(e: ReportCardEntryDb) = {
      val rDate = e.date.localDate.plusDays(1)
      val rStart = e.start.localTime.plusHours(1)
      val rEnd = e.end.localTime.plusHours(1)

      ReportCardRescheduledDb(e.id, rDate.sqlDate, rStart.sqlTime, rEnd.sqlTime, randomRoom.id, Some("some reason"))
    }

    def reportCardRetry(e: ReportCardEntryDb) = {
      val rDate = e.date.localDate.plusDays(1)
      val rStart = e.start.localTime.plusHours(1)
      val rEnd = e.end.localTime.plusHours(1)

      val id = UUID.randomUUID
      val entryTypes = takeSomeOf(PostgresReportCardEntryType.all).map { entryType =>
        ReportCardEntryTypeDb(None, Some(id), entryType.entryType)
      }.toSet

      ReportCardRetryDb(e.id, rDate.sqlDate, rStart.sqlTime, rEnd.sqlTime, randomRoom.id, entryTypes, Some("some reason"), id = id)
    }

    val student = takeOneOf(students).id
    val labwork = takeOneOf(labworks).id
    val room = randomRoom.id

    val entries = (0 until numberOfEntries).map { i =>
      val date = LocalDate.now.plusDays(i)
      val start = LocalTime.now.plusHours(i)
      val end = start.plusHours(1)

      val id = UUID.randomUUID
      val types = takeSomeOf(PostgresReportCardEntryType.all).map { entryType =>
        ReportCardEntryTypeDb(Some(id), None, entryType.entryType)
      }.toSet

      ReportCardEntryDb(student, labwork, s"assignment $i", date.sqlDate, start.sqlTime, end.sqlTime, room, types, id = id)
    }

    if (nextBoolean && withRescheduledAndRetry) {
      val rs = nextBoolean
      val rt = nextBoolean

      entries.map {
        case rescheduled if rs && !rt =>
          rescheduled.copy(rescheduled = Some(reportCardRescheduled(rescheduled)))
        case retry if !rs && rt =>
          retry.copy(retry = Some(reportCardRetry(retry)))
        case both if rs && rt =>
          both.copy(rescheduled = Some(reportCardRescheduled(both)), retry = Some(reportCardRetry(both)))
        case nothing => nothing
      }
    } else {
      entries
    }
  }.toList

  lazy val semesters = {
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

  lazy val employees = populateEmployees(maxEmployees)

  lazy val courses = (0 until maxCourses).map { i =>
    CourseDb(i.toString, i.toString, i.toString, randomEmployee.id, 1)
  }.toList

  lazy val degrees = (0 until maxDegrees).map(i => DegreeDb(i.toString, i.toString)).toList

  lazy val labworks = populateLabworks(maxLabworks)

  lazy val rooms = (0 until maxRooms).map(i => RoomDb(i.toString, i.toString)).toList

  lazy val assignmentPlans = (0 until maxAssignmentPlans).map { i =>
    val entries = (0 until 10).map { j =>
      val allTypes = PostgresAssignmentEntryType.all
      PostgresAssignmentEntry(j, j.toString, takeSomeOf(allTypes).toSet)
    }

    AssignmentPlanDb(labworks(i).id, i, i, entries.toSet)
  }.toList

  lazy val blacklists = populateBlacklists(maxBlacklists)

  lazy val timetables = populateTimetables(maxTimetables, 6)(employees, labworks.drop(1), blacklists)

  lazy val students = populateStudents(maxStudents)

  lazy val reportCardEntries = populateReportCardEntries(maxReportCardEntries, 8, withRescheduledAndRetry = false)(labworks, students)
}

abstract class AbstractDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
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
      // TODO uniqueDbEntity should have toLwmModel which should be used here for comparision
      // TODO check lastModified also
      await(update(validUpdateOnDbEntity)) shouldBe Some(validUpdateOnDbEntity)
    }

    s"create many $name" in {
      await(createMany(dbEntities)) shouldBe dbEntities
    }

    s"get many $name" in {
      // TODO we need something like this, but lwmModel trait is necessary
    }

    s"delete a $name by invalidating it" in {
      await(delete(dbEntity)) shouldBe defined
    }
  }
}
