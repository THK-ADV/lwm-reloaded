package dao

import java.util.UUID

import dao.helper.Core
import dao.helper.TableFilter.{sinceFilter, userFilter}
import database.DateStartEndTable
import database.helper.LdapUserStatus.{EmployeeStatus, StudentStatus}
import javax.inject.Inject
import models._
import org.joda.time.LocalDate
import service.dashboard.{DashboardEvaluationResult, DashboardGroupLabel}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait DashboardDao extends Core {

  protected def labworkDao: LabworkDao

  protected def labworkApplicationDao: LabworkApplicationDao

  protected def reportCardEntryDao: ReportCardEntryDao

  protected def reportCardEvaluationDao: ReportCardEvaluationDao

  protected def groupDao: GroupDao

  protected def authorityDao: AuthorityDao

  protected def scheduleEntryDao: ScheduleEntryDao

  def studentDashboard(
    student: StudentLike,
    semester: Semester,
    atomic: Boolean,
    numberOfUpcomingElements: Option[Int],
    entriesSinceNow: Boolean,
    sortedByDate: Boolean
  ): Future[StudentDashboard]

  def employeeDashboard(
    employee: User,
    semester: Semester,
    atomic: Boolean,
    numberOfUpcomingElements: Option[Int],
    entriesSinceNow: Boolean,
    sortedByDate: Boolean,
    ownEntriesOnly: Boolean
  ): Future[EmployeeDashboard]
}

final class DashboardDaoImpl @Inject()(
  val db: Database,
  val labworkDao: LabworkDao,
  val labworkApplicationDao: LabworkApplicationDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao,
  val groupDao: GroupDao,
  val authorityDao: AuthorityDao,
  val scheduleEntryDao: ScheduleEntryDao,
  val profile: JdbcProfile,
  implicit val executionContext: ExecutionContext
) extends DashboardDao {

  import profile.api._
  import utils.date.DateTimeOps._

  def studentDashboard(
    student: StudentLike,
    semester: Semester,
    atomic: Boolean,
    numberOfUpcomingElements: Option[Int],
    entriesSinceNow: Boolean,
    sortedByDate: Boolean
  ) = {
    for {
      labworks <- labworksToApplyFor(semester.id, student.enrollmentId, atomic)
      labworkIds = labworks.map(_.id)
      lapps <- labworkApplicationsFor(labworkIds, student.id, atomic)
      cards <- reportCardsSinceNow(labworkIds, student.id, entriesSinceNow, atomic)
      modify = takeReportCardEntries(numberOfUpcomingElements) andThen sortReportCardEntries(sortedByDate)
      (evals, evalResults) <- reportCardEvaluationFor(student.id)
      groupLabels <- groupsFor(student.id, labworkIds)
    } yield StudentDashboard(
      student,
      StudentStatus,
      semester,
      labworks,
      lapps,
      groupLabels,
      modify(cards),
      evals,
      evalResults
    )
  }

  def employeeDashboard(
    employee: User,
    semester: Semester,
    atomic: Boolean,
    numberOfUpcomingElements: Option[Int],
    entriesSinceNow: Boolean,
    sortedByDate: Boolean,
    ownEntriesOnly: Boolean
  ) =
    for {
      courses <- restrictedCourses(employee.id)
      scheduleEntries <- scheduleEntriesSinceNow(semester, courses.map(_.id), entriesSinceNow, if (ownEntriesOnly) Some(employee.id) else None, atomic)
      modify = takeScheduleEntries(numberOfUpcomingElements) andThen sortScheduleEntries(sortedByDate)
    } yield EmployeeDashboard(
      employee,
      EmployeeStatus,
      semester,
      courses,
      modify(scheduleEntries)
    )

  def groupsFor(student: UUID, labworks: Seq[UUID]) = {
    val query = for {
      g <- groupDao.filterValidOnly(g => g.labwork.inSet(labworks) && g.contains(student))
      l <- g.labworkFk
    } yield (g.label, l.label)

    db.run(query.result.map(_.map(t => DashboardGroupLabel(t._1, t._2))))
  }

  def labworksToApplyFor(semester: UUID, degree: UUID, atomic: Boolean) =
    labworkDao.get(List(LabworkDao.semesterFilter(semester), LabworkDao.degreeFilter(degree)), atomic)

  def labworkApplicationsFor(labworks: Seq[UUID], student: UUID, atomic: Boolean) =
    labworkApplicationDao.filter(app => app.labwork.inSet(labworks) && app.user === student, atomic)

  def reportCardsSinceNow(labworks: Seq[UUID], student: UUID, entriesSinceNow: Boolean, atomic: Boolean) = {
    val cardsQuery = reportCardEntryDao.filterValidOnly(e => e.labwork.inSet(labworks) && e.user === student && e.labworkFk.filter(_.published).exists)
    val sinceNowQuery = sinceNowFilter(entriesSinceNow).fold(cardsQuery)(cardsQuery.filter)
    reportCardEntryDao.getByQuery(sinceNowQuery, atomic = atomic)
  }

  def reportCardEvaluationFor(student: UUID): Future[(Seq[ReportCardEvaluationLike], Seq[DashboardEvaluationResult])] = {
    def accInts(acc: Int, eval: ReportCardEvaluationAtom): Int =
      if (ReportCardEntryType.IntBasedTypes.exists(_.entryType == eval.label)) acc + eval.int
      else acc

    def accBooleans(acc: Boolean, eval: ReportCardEvaluationAtom): Boolean =
      if (ReportCardEntryType.BooleanBasedTypes.exists(_.entryType == eval.label)) acc && eval.bool
      else acc

    for {
      evals <- reportCardEvaluationDao.get(List(userFilter(student))) // must be atomic
      evalResults = evals
        .groupBy(_.labworkId)
        .map {
          case (_, evalLike) =>
            val atoms = evalLike.map(_.asInstanceOf[ReportCardEvaluationAtom])
            val labwork = atoms.head.labwork

            val (bonus, passed) = atoms.foldLeft((0, true)) { (acc, eval) =>
              (accInts(acc._1, eval), accBooleans(acc._2, eval))
            }

            DashboardEvaluationResult(
              labwork.course.abbreviation,
              labwork.semester.abbreviation,
              passed,
              bonus
            )
        }
    } yield evals -> evalResults.toSeq
  }

  def restrictedCourses(userId: UUID) = for {
    authorities <- authorityDao.get(List(userFilter(userId))) // must be atomic ...
    courses = authorities.flatMap(_.asInstanceOf[AuthorityAtom].course) // in order to get his/her courses
  } yield courses

  def sortScheduleEntries(sortedByDate: Boolean)(entries: Seq[ScheduleEntryLike]) =
    if (sortedByDate) entries sortBy (s => (s.date, s.start)) else entries

  def sortReportCardEntries(sortedByDate: Boolean)(entries: Seq[ReportCardEntryLike]) =
    if (sortedByDate) entries sortBy (s => (s.date, s.start)) else entries

  def takeScheduleEntries(n: Option[Int]): Seq[ScheduleEntryLike] => Seq[ScheduleEntryLike] =
    take[ScheduleEntryLike](n) {
      case (n, entries) => entries.groupBy(_.labworkId).flatMap(_._2.take(n)).toSeq
    }

  def takeReportCardEntries(n: Option[Int]): Seq[ReportCardEntryLike] => Seq[ReportCardEntryLike] =
    take[ReportCardEntryLike](n) {
      case (n, entries) => entries.groupBy(_.labworkId).flatMap(_._2.take(n)).toSeq
    }

  def take[A](numberOfEntries: Option[Int])(f: (Int, Seq[A]) => Seq[A])(seq: Seq[A]) =
    numberOfEntries.fold(seq)(n => f(n, seq))

  def scheduleEntriesSinceNow(
    semester: Semester,
    courses: Seq[UUID],
    entriesSinceNow: Boolean,
    filterSupervisor: Option[UUID],
    atomic: Boolean
  ) = {
    val entriesQuery = scheduleEntryDao.filterValidOnly(q => q.labworkFk.filter(_.semester === semester.id).exists && q.memberOfCourses(courses))
    val ownEntriesQuery = filterSupervisor.fold(entriesQuery)(user => entriesQuery.filter(_.containsSupervisor(user)))
    val sinceNowQuery = sinceNowFilter(entriesSinceNow).fold(ownEntriesQuery)(ownEntriesQuery.filter)
    scheduleEntryDao.getByQuery(sinceNowQuery, atomic = atomic)
  }

  private def sinceNowFilter(entriesSinceNow: Boolean): Option[DateStartEndTable => Rep[Boolean]] =
    if (entriesSinceNow) Option(sinceFilter(LocalDate.now.sqlDate)) else Option.empty
}