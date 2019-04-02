package dao

import java.util.UUID

import dao.helper.Core
import database.helper.{EmployeeStatus, StudentStatus}
import javax.inject.Inject
import models.{AuthorityAtom, Dashboard, EmployeeDashboard, Semester, Student, StudentDashboard, User}
import org.joda.time.LocalDate
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

import scala.concurrent.{ExecutionContext, Future}

trait DashboardDao extends Core { // TODO continue

  def dashboard(systemId: String)(atomic: Boolean, numberOfUpcomingElements: Option[Int], entriesSinceNow: Boolean): Future[Dashboard] = {
    for {
      maybeSemester <- semesterDao.getSingle(UUID.fromString("0496eec7-ad06-4d82-86e1-ad7782dcd92b")) //semesterDao.current(atomic)
      semester <- maybeSemester.fold(throw new Throwable(s"none or more than one semester was found, which is marked as the current semester"))(Future.successful)
      user <- userDao.getSingleWhere(_.systemId === systemId, atomic = false)
      board <- user match {
        case Some(student: Student) =>
          studentDashboard(student, semester)(atomic, numberOfUpcomingElements, entriesSinceNow)
        case Some(employee) =>
          employeeDashboard(employee, semester)(atomic, numberOfUpcomingElements, entriesSinceNow)
        case None =>
          throw new Throwable(s"no user found for $systemId")
      }
    } yield board
  }

  private def studentDashboard(student: Student, semester: Semester)(atomic: Boolean, numberOfUpcomingElements: Option[Int], entriesSinceNow: Boolean) = { // current semester
    //    for {
    //      labworks <- labworkDao.filter(l => l.semester === semester.id && l.degree === enrollment, atomic, validOnly, sinceLastModified)
    //      labworIds = labworks.map(_.id)
    //
    //      apps <- labworkApplicationDao.filter(app => app.applicant === student && app.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified) // split into passed/not passed & filter not published
    //      groups <- groupDao.filter(g => g.contains(student) && g.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
    //      cards <- reportCardEntryDao.filter(e => e.student === student && e.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified) // labwork is published & take first n upcoming elements
    //      evals <- reportCardEvaluationDao.filter(_.student === student, atomic, validOnly, sinceLastModified) // with semester and combine with patterns
    //      evalPatterns <- reportCardEvaluationPatternDao.filter(_.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
    //    } yield StudentDashboard(semester, labworks, apps, groups, cards, evals, evalPatterns)
    Future.successful(StudentDashboard(student, StudentStatus, semester, Traversable.empty, Traversable.empty, Traversable.empty, Traversable.empty, Traversable.empty, Traversable.empty))
  }

  private def employeeDashboard(employee: User, semester: Semester)(atomic: Boolean, numberOfUpcomingElements: Option[Int], entriesSinceNow: Boolean) = {
    for {
      authorities <- authorityDao.get(List(AuthorityUserFilter(employee.id.toString))) // must be atomic
      courses = authorities.flatMap(_.asInstanceOf[AuthorityAtom].course)
      courseIds = courses.map(_.id)

      now = ScheduleEntrySinceFilter(LocalDate.now.stringMillis).predicate
      currentEntries = scheduleEntryDao
        .filterValidOnly(q => q.labworkFk.filter(_.semester === semester.id).exists && q.memberOfCourses(courseIds) && (if (entriesSinceNow) now.apply(q) else true))
        .sortBy(q => (q.date.asc, q.start.asc))

      scheduleEntries <- scheduleEntryDao.getByQuery(currentEntries, atomic = atomic)
      upcomingEntries = numberOfUpcomingElements.fold(scheduleEntries)(n => scheduleEntries.groupBy(_.labworkId).flatMap(_._2.take(n)))
    } yield EmployeeDashboard(employee, EmployeeStatus, semester, courses, upcomingEntries)
  }

  protected def userDao: UserDao

  protected def semesterDao: SemesterDao

  protected def labworkDao: LabworkDao

  protected def labworkApplicationDao: LabworkApplicationDao

  protected def reportCardEntryDao: ReportCardEntryDao

  protected def reportCardEvaluationDao: ReportCardEvaluationDao

  protected def reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao

  protected def groupDao: GroupDao

  protected def authorityDao: AuthorityDao

  protected def scheduleEntryDao: ScheduleEntryDao
}

final class DashboardDaoImpl @Inject()(
  val db: Database,
  val userDao: UserDao,
  val semesterDao: SemesterDao,
  val labworkDao: LabworkDao,
  val labworkApplicationDao: LabworkApplicationDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao,
  val reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao,
  val groupDao: GroupDao,
  val authorityDao: AuthorityDao,
  val scheduleEntryDao: ScheduleEntryDao,
  val executionContext: ExecutionContext
) extends DashboardDao