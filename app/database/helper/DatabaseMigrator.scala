package database.helper

import java.util.UUID

import com.google.inject.Inject
import dao._
import dao.helper.Core
import database._
import models._
import org.joda.time.{LocalDate, LocalTime}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait DatabaseMigrator extends Core with DatabaseTables {

  import utils.date.DateTimeOps._

  def userDao: UserDao

  def degreeDao: DegreeDao

  def roleDao: RoleDao

  def semesterDao: SemesterDao

  def roomDao: RoomDao

  def courseDao: CourseDao

  def labworkDao: LabworkDao

  def lappDao: LabworkApplicationDao

  def assignmentDao: AssignmentEntryDao

  def blacklistDao: BlacklistDao

  def timetableDao: TimetableDao

  def reportCardEntryDao: ReportCardEntryDao

  def authorityDao: AuthorityDao

  def groupDao: GroupDao

  def scheduleEntryDao: ScheduleEntryDao

  def reportCardEvaluationDao: ReportCardEvaluationDao

  def createDatabase: Future[Unit] = db.run(createAction())

  def dropDatabase: Future[Unit] = db run dropAction()

  def migrateStudents(students: List[Student]) = {
    userDao.createMany(students.map(toDbUser)).map(_.map(_.toUniqueEntity))
  }

  def migrateEmployees(employees: List[Employee]) = {
    userDao.createMany(employees.map(toDbUser)).map(_.map(_.toUniqueEntity))
  }

  def migrateDegrees(degrees: List[Degree]) = {
    degreeDao.createMany(degrees.map(toDbDegree)).map(_.map(_.toUniqueEntity))
  }

  def migrateRoles(roles: List[Role]) = {
    roleDao.createMany(roles.map(toDbRole)).map(_.map(_.toUniqueEntity))
  }

  def migrateSemesters(semesters: List[Semester]) = {
    semesterDao.createMany(semesters.map(toSemesterDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateCourses(courses: List[Course]) = {
    courseDao.createMany(courses.map(toCourseDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateLabworks(labworks: List[Labwork]) = {
    labworkDao.createMany(labworks.map(toLabworkDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateRooms(rooms: List[Room]) = {
    roomDao.createMany(rooms.map(toRoomDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateLabworkApplications(apps: List[LabworkApplication]) = {
    lappDao.createMany(apps.map(toAppDb)).map(_.map(_.toUniqueEntity))
  }

  type AssignmentPlan0 = (UUID, List[(Int, String, Set[String], Int)])

  def migrateAssignmentPlans(plans: List[AssignmentPlan0]) = {
    assignmentDao.createMany(plans.flatMap(p => toAssignmentPlanEntries(p._1, p._2))).map(_.map(_.toUniqueEntity))
  }

  type TimetableEntry0 = (Set[UUID], UUID, Int, LocalTime, LocalTime)
  type Timetable0 = (UUID, Set[TimetableEntry0], LocalDate, UUID)

  def migrateTimetables(timetables: List[Timetable0]) = {
    timetableDao.createMany(timetables.map(t => toTimetables(t._1, t._2, t._3, t._4))).map(_.map(_.toUniqueEntity))
  }

  def migrateReportCardEntries(entries: List[ReportCardEntry]) = {
    reportCardEntryDao.createMany(entries.map(toReportCardEntryDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateAuthorities(auths: List[Authority]) = {
    authorityDao.createMany(auths.map(toAuthorityDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateGroups(groups: List[Group]) = {
    groupDao.createMany(groups.map(toGroupDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateScheduleEntries(entries: List[ScheduleEntry]) = {
    scheduleEntryDao.createMany(entries.map(toScheduleEntryDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateReportCardEvaluations(evals: List[ReportCardEvaluation]) = {
    reportCardEvaluationDao.createMany(evals.map(toReportCardEvaluationDb)).map(_.map(_.toUniqueEntity))
  }

  def migrateReportCardEvaluationPatterns() = {
    Future.successful(Seq.empty[ReportCardEvaluationPatternDb]).map(_.map(_.toUniqueEntity))
  }

  private def toDbUser(user: User): UserDb = user match {
    case Student(systemId, lastname, firstname, email, registrationId, enrollment, id) =>
      UserDb(systemId, lastname, firstname, email, LdapUserStatus.StudentStatus, Some(registrationId), Some(enrollment), id = id)
    case Lecturer(systemId, lastname, firstname, email, id) =>
      UserDb(systemId, lastname, firstname, email, LdapUserStatus.LecturerStatus, None, None, id = id)
    case Employee(systemId, lastname, firstname, email, id) =>
      UserDb(systemId, lastname, firstname, email, LdapUserStatus.EmployeeStatus, None, None, id = id)
  }

  private def toDbDegree(degree: Degree) =
    DegreeDb(degree.label, degree.abbreviation, id = degree.id)

  private def toDbRole(r: Role) =
    RoleDb(r.label, id = r.id)

  private def toSemesterDb(s: Semester) =
    SemesterDb(s.label, s.abbreviation, s.start.sqlDate, s.end.sqlDate, s.examStart.sqlDate, id = s.id)

  private def toCourseDb(c: Course) =
    CourseDb(c.label, c.description, c.abbreviation, c.lecturer, c.semesterIndex, id = c.id)

  private def toLabworkDb(l: Labwork) =
    LabworkDb(l.label, l.description, l.semester, l.course, l.degree, l.subscribable, l.published, id = l.id)

  private def toRoomDb(r: Room) =
    RoomDb(r.label, r.description, r.capacity, id = r.id)

  private def toAppDb(a: LabworkApplication) =
    LabworkApplicationDb(a.labwork, a.applicant, a.friends, a.lastModified.timestamp, id = a.id)

  private def toAssignmentPlanEntries(labwork: UUID, entries: List[(Int, String, Set[String], Int)]) = entries.map {
    case (index, label, types, duration) =>
      val id = UUID.randomUUID
      AssignmentEntryDb(labwork, index, label, types.map(AssignmentEntryTypeDb(id, _)), duration, id = id)
  }

  private def toTimetables(labwork: UUID, entries: Set[TimetableEntry0], start: LocalDate, id: UUID) = {
    def toTimetableEntry(e: TimetableEntry0) =
      TimetableEntry(e._1, e._2, e._3, e._4, e._5)

    TimetableDb(labwork, entries.map(toTimetableEntry), start.sqlDate, Set.empty, id = id)
  }

  private def toReportCardEntryDb(e: ReportCardEntry) = {
    ReportCardEntryDb(
      e.student,
      e.labwork,
      e.label,
      e.date.sqlDate,
      e.start.sqlTime,
      e.end.sqlTime,
      e.room,
      e.entryTypes.map(t => ReportCardEntryTypeDb(Some(e.id), None, t.entryType, t.bool, t.int, id = t.id)),
      e.assignmentIndex,
      e.rescheduled.map(t => ReportCardRescheduledDb(e.id, t.date.sqlDate, t.start.sqlTime, t.end.sqlTime, t.room, t.reason, id = t.id)),
      None,
      id = e.id
    )
  }

  private def toAuthorityDb(a: Authority) =
    AuthorityDb(a.user, a.role, a.course, id = a.id)

  private def toGroupDb(g: Group) =
    GroupDb(g.label, g.labwork, g.members, id = g.id)

  private def toScheduleEntryDb(e: ScheduleEntry) =
    ScheduleEntryDb(e.labwork, e.start.sqlTime, e.end.sqlTime, e.date.sqlDate, e.room, e.supervisor, e.group, id = e.id)

  private def toReportCardEvaluationDb(e: ReportCardEvaluation) =
    ReportCardEvaluationDb(e.student, e.labwork, e.label, e.bool, e.int, id = e.id)
}

final class DatabaseMigratorImpl @Inject()(
  val db: Database,
  val executionContext: ExecutionContext,
  val userDao: UserDao,
  val degreeDao: DegreeDao,
  val roleDao: RoleDao,
  val semesterDao: SemesterDao,
  val roomDao: RoomDao,
  val courseDao: CourseDao,
  val labworkDao: LabworkDao,
  val lappDao: LabworkApplicationDao,
  val assignmentDao: AssignmentEntryDao,
  val blacklistDao: BlacklistDao,
  val timetableDao: TimetableDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val authorityDao: AuthorityDao,
  val groupDao: GroupDao,
  val scheduleEntryDao: ScheduleEntryDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao
) extends DatabaseMigrator {
  override implicit val profile: JdbcProfile = _root_.slick.jdbc.PostgresProfile
}