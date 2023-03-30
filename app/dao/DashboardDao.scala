package dao

import dao.helper.Core
import dao.helper.TableFilter.{sinceFilter, userFilter}
import database.DateStartEndTable
import database.helper.LdapUserStatus.{EmployeeStatus, StudentStatus}
import models._
import org.joda.time.LocalDate
import service.dashboard.{DashboardEvaluationResult, DashboardGroupLabel}
import slick.jdbc.JdbcProfile
import slick.jdbc.PostgresProfile.api._

import java.util.UUID
import javax.inject.Inject
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
      sortedByDate: Boolean,
      ownEntriesOnly: Boolean
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

final class DashboardDaoImpl @Inject() (
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
      sortedByDate: Boolean,
      ownEntriesOnly: Boolean
  ): Future[StudentDashboard] = {
    for {
      labworks <- labworksToApplyFor(semester.id, student.enrollmentId, atomic)
      lapps <- labworkApplicationsFor(semester.id, student.id, atomic)
      cards <- reportCardsSinceNow(
        semester.id,
        student.id,
        entriesSinceNow,
        atomic
      )
      modify = takeReportCardEntries(
        numberOfUpcomingElements
      ) andThen sortReportCardEntries(sortedByDate)
      evalResults <- reportCardEvaluationFor(student.id)
      groupLabels <- groupsFor(student.id, semester.id)
      courses <- restrictedCourses(student.id)
      scheduleEntries <- scheduleEntries(
        student.id,
        semester,
        courses.map(_.id),
        entriesSinceNow,
        ownEntriesOnly,
        sortedByDate,
        numberOfUpcomingElements,
        atomic
      )
    } yield StudentDashboard(
      student,
      StudentStatus,
      semester,
      labworks,
      lapps,
      groupLabels,
      modify(cards),
      evalResults,
      scheduleEntries
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
  ): Future[EmployeeDashboard] =
    for {
      courses <- restrictedCourses(employee.id)
      scheduleEntries <- scheduleEntries(
        employee.id,
        semester,
        courses.map(_.id),
        entriesSinceNow,
        ownEntriesOnly,
        sortedByDate,
        numberOfUpcomingElements,
        atomic
      )
    } yield EmployeeDashboard(
      employee,
      EmployeeStatus,
      semester,
      courses,
      scheduleEntries
    )

  def groupsFor(
      student: UUID,
      semester: UUID
  ): Future[Seq[DashboardGroupLabel]] = {
    val query = for {
      g <- groupDao.filterValidOnly(g =>
        g.inSemester(semester) && g.contains(student)
      )
      l <- g.labworkFk
    } yield (g.label, l.label, l.id)

    db.run(query.result.map(_.map(t => DashboardGroupLabel(t._1, t._2, t._3))))
  }

  def labworksToApplyFor(
      semester: UUID,
      degree: UUID,
      atomic: Boolean
  ): Future[Seq[LabworkLike]] =
    labworkDao.get(
      List(
        LabworkDao.semesterFilter(semester),
        LabworkDao.degreeFilter(degree)
      ),
      atomic
    )

  def labworkApplicationsFor(
      semester: UUID,
      student: UUID,
      atomic: Boolean
  ): Future[Seq[LabworkApplicationLike]] =
    labworkApplicationDao.filter(
      app => app.inSemester(semester) && app.user === student,
      atomic
    )

  def reportCardsSinceNow(
      semester: UUID,
      student: UUID,
      entriesSinceNow: Boolean,
      atomic: Boolean
  ): Future[Seq[(ReportCardEntryLike, Set[ReportCardRescheduledLike])]] = {
    val cardsQuery = reportCardEntryDao.filterValidOnly(e =>
      e.user === student &&
        e.inSemester(semester) &&
        e.labworkFk.filter(_.published).exists
    )
    val sinceNowQuery =
      sinceNowFilter(entriesSinceNow).fold(cardsQuery)(cardsQuery.filter)
    reportCardEntryDao.withReschedules(sinceNowQuery, atomic = atomic)
  }

  def reportCardEvaluationFor(
      student: UUID
  ): Future[Seq[DashboardEvaluationResult]] =
    for {
      evals <- reportCardEvaluationDao.get(
        List(userFilter(student))
      ) // must be atomic
      evalResults = evals
        .groupBy(_.labworkId)
        .map { case (_, evalLike) =>
          val atoms = evalLike.map(_.asInstanceOf[ReportCardEvaluationAtom])
          val labwork = atoms.head.labwork

          if (atoms.isEmpty)
            DashboardEvaluationResult(
              labwork.course.abbreviation,
              labwork.semester.abbreviation,
              passed = false,
              0
            )
          else {
            val (bonus, passed) = atoms.foldLeft((0, true)) { (acc, eval) =>
              (acc._1 + eval.int, acc._2 && eval.bool)
            }

            DashboardEvaluationResult(
              labwork.course.abbreviation,
              labwork.semester.abbreviation,
              passed,
              bonus
            )
          }
        }
    } yield evalResults.toSeq

  def restrictedCourses(userId: UUID): Future[Seq[CourseAtom]] = for {
    authorities <- authorityDao.get(
      List(userFilter(userId))
    ) // must be atomic ...
    courses = authorities.flatMap(
      _.asInstanceOf[AuthorityAtom].course
    ) // in order to get his/her courses
  } yield courses

  def sortScheduleEntries(sortedByDate: Boolean)(
      entries: Seq[ScheduleEntryLike]
  ): Seq[ScheduleEntryLike] =
    if (sortedByDate) entries sortBy (s => (s.date, s.start)) else entries

  def sortReportCardEntries(sortedByDate: Boolean)(
      entries: Seq[(ReportCardEntryLike, Set[ReportCardRescheduledLike])]
  ): Seq[(ReportCardEntryLike, Set[ReportCardRescheduledLike])] =
    if (sortedByDate) entries sortBy (s => (s._1.date, s._1.start)) else entries

  def takeScheduleEntries(
      n: Option[Int]
  ): Seq[ScheduleEntryLike] => Seq[ScheduleEntryLike] =
    take[ScheduleEntryLike](n) { case (n, entries) =>
      entries.groupBy(_.labworkId).flatMap(_._2.take(n)).toSeq
    }

  def takeReportCardEntries(
      n: Option[Int]
  ): Seq[(ReportCardEntryLike, Set[ReportCardRescheduledLike])] => Seq[
    (ReportCardEntryLike, Set[ReportCardRescheduledLike])
  ] =
    take[(ReportCardEntryLike, Set[ReportCardRescheduledLike])](n) {
      case (n, entries) =>
        entries.groupBy(_._1.labworkId).flatMap(_._2.take(n)).toSeq
    }

  def take[A](numberOfEntries: Option[Int])(f: (Int, Seq[A]) => Seq[A])(
      seq: Seq[A]
  ): Seq[A] =
    numberOfEntries.fold(seq)(n => f(n, seq))

  def scheduleEntries(
      user: UUID,
      semester: Semester,
      courses: Seq[UUID],
      entriesSinceNow: Boolean,
      ownEntriesOnly: Boolean,
      sortedByDate: Boolean,
      numberOfUpcomingElements: Option[Int],
      atomic: Boolean
  ): Future[Seq[ScheduleEntryLike]] = for {
    scheduleEntries <- scheduleEntriesSinceNow(
      semester,
      courses,
      entriesSinceNow,
      if (ownEntriesOnly) Some(user) else None,
      atomic
    )
    modified =
      if (scheduleEntries.nonEmpty)
        (takeScheduleEntries(
          numberOfUpcomingElements
        ) andThen sortScheduleEntries(sortedByDate))(scheduleEntries)
      else
        scheduleEntries
  } yield modified

  def scheduleEntriesSinceNow(
      semester: Semester,
      courses: Seq[UUID],
      entriesSinceNow: Boolean,
      filterSupervisor: Option[UUID],
      atomic: Boolean
  ): Future[Seq[ScheduleEntryLike]] = {
    val entriesQuery = scheduleEntryDao.filterValidOnly(q =>
      q.labworkFk.filter(_.semester === semester.id).exists && q
        .memberOfCourses(courses)
    )
    val ownEntriesQuery = filterSupervisor.fold(entriesQuery)(user =>
      entriesQuery.filter(_.containsSupervisor(user))
    )
    val sinceNowQuery = sinceNowFilter(entriesSinceNow).fold(ownEntriesQuery)(
      ownEntriesQuery.filter
    )
    scheduleEntryDao.getByQuery(sinceNowQuery, atomic = atomic)
  }

  private def sinceNowFilter(
      entriesSinceNow: Boolean
  ): Option[DateStartEndTable => Rep[Boolean]] =
    if (entriesSinceNow) Option(sinceFilter(LocalDate.now.sqlDate))
    else Option.empty
}
