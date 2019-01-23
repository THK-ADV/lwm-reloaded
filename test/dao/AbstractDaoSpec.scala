package dao

import java.sql.Timestamp
import java.util.UUID

import base.PostgresDbSpec
import utils.LwmDateTime._
import models._
import org.joda.time.{LocalDate, LocalTime}
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._
import database._

object AbstractDaoSpec {
  import scala.util.Random.{nextBoolean, nextInt, shuffle}

  lazy val maxDegrees = 10
  lazy val maxLabworks = 20
  lazy val maxSemesters = 10
  lazy val maxCourses = 10
  lazy val maxRooms = 10
  lazy val maxEmployees = 10
  lazy val maxAssignmentPlans = 10
  lazy val maxBlacklists = 100
  lazy val maxTimetables = 10
  lazy val maxStudents = 100
  lazy val maxReportCardEntries = 100
  lazy val maxPermissions = 10
  lazy val maxAuthorities = 10
  lazy val maxScheduleEntries = 100
  lazy val maxGroups = 20
  lazy val maxEvaluations = 100
  lazy val maxLabworkApplications = 20
  lazy val maxEvaluationPatterns = 4 * 5

  def randomSemester = semesters(nextInt(maxSemesters))
  def randomCourse = courses(nextInt(maxCourses))
  def randomDegree = degrees(nextInt(maxDegrees))
  def randomLabwork = labworks(nextInt(maxDegrees))
  def randomRoom = rooms(nextInt(maxRooms))
  def randomEmployee = employees(nextInt(maxEmployees))
  def randomBlacklist = blacklists(nextInt(maxBlacklists))
  def randomStudent = students(nextInt(maxStudents))
  def randomPermission = permissions(nextInt(maxPermissions))
  def randomRole = roles(nextInt(roles.length))
  def randomAuthority = authorities(nextInt(maxAuthorities))
  def randomGroup = groups(nextInt(maxGroups))
  def randomReportCardEntryTypes(reportCardEntry: Option[UUID], reportCardRetry: Option[UUID]) = takeSomeOf(ReportCardEntryType.all).map { entryType =>
    ReportCardEntryTypeDb(reportCardEntry, reportCardRetry, entryType.entryType)
  }.toSet


  final def takeSomeOf[A](traversable: Traversable[A]) = if (traversable.isEmpty) traversable else traversable.take(nextInt(traversable.size - 1) + 1)

  final def takeOneOf[A](traversable: Traversable[A]) = shuffle(traversable).head

  final def populateBlacklists(amount: Int) = (0 until amount).map { i =>
    val global = nextBoolean

    val (date, start, end) = {
      val date = LocalDate.now.plusDays(i)

      if (global) {
        (date, Blacklist.startOfDay, Blacklist.endOfDay)
      } else {
        val start = LocalTime.now.withHourOfDay(nextInt(23))
        val end = start.plusHours(1)
        (date, start, end)
      }
    }

    BlacklistDb(i.toString, date.sqlDate, start.sqlTime, end.sqlTime, global)
  }.toList

  final def populateLabworks(amount: Int)(semesters: List[SemesterDb], courses: List[CourseDb], degrees: List[DegreeDb]) = (0 until amount).map { i =>
    LabworkDb(i.toString, i.toString, takeOneOf(semesters).id, takeOneOf(courses).id, takeOneOf(degrees).id)
  }.toList

  final def populateEmployees(amount: Int) = (0 until amount).map { i =>
    UserDb(i.toString, i.toString, i.toString, i.toString, User.EmployeeType, None, None)
  }.toList

  final def populateStudents(amount: Int) = (0 until amount).map { i =>
    UserDb(i.toString, i.toString, i.toString, i.toString, User.StudentType, Some(i.toString), Some(randomDegree.id))
  }.toList

  final def populateTimetables(amount: Int, numberOfEntries: Int)(users: List[UserDb], labworks: List[LabworkDb], blacklists: List[BlacklistDb]) = (0 until amount).map { i =>
    val entries = (0 until numberOfEntries).map { j =>
      TimetableEntry(takeSomeOf(users).map(_.id).toSet, randomRoom.id, nextInt(5), LocalTime.now.plusHours(j), LocalTime.now.plusHours(j + 1))
    }

    TimetableDb(labworks(i).id, entries.toSet, LocalDate.now.plusDays(i).sqlDate, takeSomeOf(blacklists).map(_.id).toSet)
  }.toList

  final def populateGroups(amount: Int)(labworks: List[LabworkDb], students: List[UserDb]) = (0 until amount).map { i =>
    GroupDb(i.toString, takeOneOf(labworks).id, takeSomeOf(students).map(_.id).toSet)
  }.toList

  final def populateDegrees(amount: Int) = (0 until amount).map(i => DegreeDb(i.toString, i.toString)).toList

  final def populateCourses(amount: Int)(semesterIndex: (Int) => Int) = (0 until amount).map { i =>
    CourseDb(i.toString, i.toString, i.toString, randomEmployee.id, semesterIndex(i))
  }.toList

  final def populateSemester(amount: Int) = {
    val template = LocalDate.now.withDayOfWeek(1).withMonthOfYear(9).minusYears(5).plusMonths(6)

    (0 until amount).foldLeft((List.empty[SemesterDb], template)) {
      case ((list, t), i) =>
        val start = t.plusDays(1)
        val end = start.plusMonths(6)
        val exam = end.minusMonths(1)

        val current = SemesterDb(i.toString, i.toString, start.sqlDate, end.sqlDate, exam.sqlDate)
        (list.:+(current), end)
    }._1
  }

  final def populateAssignmentPlans(amount: Int, numberOfEntries: Int)(labworks: List[LabworkDb])(duration: (Int) => Int) = (0 until amount).map { i =>
    val entries = (0 until numberOfEntries).map { j =>
      val allTypes = AssignmentEntryType.all
      AssignmentEntry(j, j.toString, takeSomeOf(allTypes).toSet, duration(j))
    }

    AssignmentPlanDb(labworks(i).id, i, i, entries.toSet)
  }.toList

  def populateScheduleEntry(amount: Int)(labworks: List[LabworkDb], rooms: List[RoomDb], employees: List[UserDb], groups: List[GroupDb]) = {
    val labwork = takeOneOf(labworks).id

    (0 until amount).map { i =>
      val date = LocalDate.now.plusDays(i)
      val start = LocalTime.now.plusHours(i)
      val end = start.plusHours(1)

      ScheduleEntryDb(labwork, start.sqlTime, end.sqlTime, date.sqlDate, takeOneOf(rooms).id, takeSomeOf(employees).map(_.id).toSet, takeOneOf(groups).id)
    }
  }.toList

  def populateReportCardEntries(amount: Int, numberOfEntries: Int, withRescheduledAndRetry: Boolean)(labworks: List[LabworkDb], students: List[UserDb]) = (0 until amount).flatMap { _ =>
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
      val entryTypes = randomReportCardEntryTypes(None, Some(id))

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
      val types = randomReportCardEntryTypes(Some(id), None)

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

  def populateReportCardEvaluations(amount: Int, numberOfEntries: Int)(students: List[UserDb], labworks: List[LabworkDb]) = (0 until amount).flatMap { _ =>
    val labwork = takeOneOf(labworks).id

    (0 until numberOfEntries).map(i => ReportCardEvaluationDb(takeOneOf(students).id, labwork, i.toString, nextBoolean, nextInt(10)))
  }.toList

  @scala.annotation.tailrec
  def randomStudent(avoiding: UUID, applicants: List[UserDb]): UUID = {
    if (applicants.forall(_ == avoiding))
      avoiding
    else {
      val app = takeOneOf(applicants).id
      if (app == avoiding) randomStudent(avoiding, applicants) else app
    }
  }

  // does not care about business rules such as only one applicant per labwork
  def populateLabworkApplications(amount: Int, withFriends: Boolean)(labworks: List[LabworkDb], applicants: List[UserDb]) = (0 until amount).map { _ =>
    val applicant = takeOneOf(applicants).id
    val friends = if (withFriends) Set(randomStudent(applicant, applicants)) else Set.empty[UUID]
    database.LabworkApplicationDb(takeOneOf(labworks).id, applicant, friends)
  }.toList

  def populateEvaluationPatterns(amount: Int)(labworks: List[LabworkDb]) = (0 until amount).map { i =>
    ReportCardEvaluationPatternDb(takeOneOf(labworks).id, i.toString, nextInt(10) + 1, (if (nextBoolean) BoolBased else IntBased).toString)
  }.toList

  lazy val semesters = populateSemester(maxSemesters)

  lazy val employees = populateEmployees(maxEmployees)

  lazy val courses = populateCourses(maxCourses)(_ % 6)

  lazy val degrees = populateDegrees(maxDegrees)

  lazy val authorities = (0 until maxAuthorities).map{ i =>
    val role: RoleDb = roles((i * 3) % roles.length)
    val course: Option[UUID] = if (role.label == Roles.RightsManagerLabel) Some(courses((i * 6) % maxCourses).id) else None
    AuthorityDb(employees( i % maxEmployees).id, role.id, course)
  }.toList

  lazy val roles = Roles.all.map(RoleDb(_))

  lazy val labworks = populateLabworks(maxLabworks)(semesters, courses, degrees)

  lazy val rooms = (0 until maxRooms).map(i => RoomDb(i.toString, i.toString, i)).toList

  lazy val assignmentPlans = populateAssignmentPlans(maxAssignmentPlans, 10)(labworks)(_ => 1)

  lazy val blacklists = populateBlacklists(maxBlacklists)

  lazy val timetables = populateTimetables(maxTimetables, 6)(employees, labworks.drop(1), blacklists)

  lazy val students = populateStudents(maxStudents)

  lazy val reportCardEntries = populateReportCardEntries(maxReportCardEntries, 8, withRescheduledAndRetry = false)(labworks, students)

  lazy val permissions = (0 until maxPermissions).map { i =>
    PermissionDb(s"label$i", s"description$i")
  }.toList

  lazy val groups = populateGroups(maxGroups)(labworks, students) // remember to add groupMemberships also

  lazy val groupMemberships = groups.flatMap(g => g.members.map(m => GroupMembership(g.id, m)))

  lazy val scheduleEntries = populateScheduleEntry(maxScheduleEntries)(labworks, rooms, employees, groups)

  lazy val reportCardEvaluations = populateReportCardEvaluations(maxEvaluations, 4)(students, labworks)

  lazy val labworkApplications = populateLabworkApplications(maxLabworkApplications, withFriends = true)(labworks, students)

  lazy val reportCardEvaluationpatterns = populateEvaluationPatterns(maxEvaluationPatterns)(labworks)
}

abstract class AbstractDaoSpec[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
  extends PostgresDbSpec with AbstractDao[T, DbModel, LwmModel] {

  protected val lastModified: Timestamp = {
    import utils.LwmDateTime.DateTimeConverter
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
