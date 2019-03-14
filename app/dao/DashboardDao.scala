package dao

import java.util.UUID

import dao.helper.Core
import javax.inject.Inject
import models.{AuthorityAtom, EmployeeDashboard, Semester, Student, StudentDashboard}
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

import scala.concurrent.{ExecutionContext, Future}

trait DashboardDao extends Core { // TODO continue

  def dashboard(systemId: String)(atomic: Boolean) = {
    for {
      maybeSemester <- semesterDao.current(atomic)
      semester <- maybeSemester.fold(throw new Throwable(s"none or more than one semester was found, which is marked as the current semester"))(Future.successful)
      user <- userDao.getSingleWhere(_.systemId === systemId, atomic = false)
      board <- user match {
        case Some(Student(_, _, _, _, _, enrollment, id)) =>
          studentDashboard(id, enrollment, semester)(atomic)
        case Some(employee) =>
          employeeDashboard(employee.id, semester)(atomic)
        case None =>
          throw new Throwable(s"no user found for $systemId")
      }
    } yield board
  }

  private def studentDashboard(student: UUID, enrollment: UUID, semester: Semester)(atomic: Boolean) = {
//    for {
//      labworks <- labworkDao.filter(l => l.semester === semester.id && l.degree === enrollment, atomic, validOnly, sinceLastModified)
//      labworIds = labworks.map(_.id)
//
//      apps <- labworkApplicationDao.filter(app => app.applicant === student && app.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
//      groups <- groupDao.filter(g => g.contains(student) && g.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
//      cards <- reportCardEntryDao.filter(e => e.student === student && e.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
//      evals <- reportCardEvaluationDao.filter(_.student === student, atomic, validOnly, sinceLastModified)
//      evalPatterns <- reportCardEvaluationPatternDao.filter(_.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
//    } yield StudentDashboard(semester, labworks, apps, groups, cards, evals, evalPatterns)
    Future.successful(???)
  }

  private def employeeDashboard(employee: UUID, semester: Semester)(atomic: Boolean) = {
    for {
      auths <- authorityDao.get(List(AuthorityUserFilter(employee.toString))) // must be atomic
      courses = auths.flatMap(_.asInstanceOf[AuthorityAtom].course)

      scheduleEntryFilter = List(ScheduleEntrySupervisorFilter(employee.toString), ScheduleEntrySinceFilter(semester.start.stringMillis), ScheduleEntryUntilFilter(semester.end.stringMillis))
      scheduleEntries <- scheduleEntryDao.get(scheduleEntryFilter, atomic)
    } yield EmployeeDashboard(semester, courses, scheduleEntries)
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