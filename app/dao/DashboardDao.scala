package dao

import java.util.UUID

import javax.inject.Inject
import models.{EmployeeDashboard, AuthorityAtom, Semester, Student, StudentDashboard}
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

import scala.concurrent.Future

trait DashboardDao {

  import scala.concurrent.ExecutionContext.Implicits.global

  def dashboard(systemId: String)(atomic: Boolean, validOnly: Boolean, sinceLastModified: Option[String]) = {
    semesterDao.get(List(SemesterCurrentFilter()), atomic, validOnly, sinceLastModified) map (_.toList) flatMap {
      case head :: Nil =>
        userDao.get(List(UserSystemIdFilter(systemId)), atomic = false, validOnly, sinceLastModified).map (_.headOption) flatMap {
          case Some(Student(_, _, _, _, _, enrollment, id)) => student(id, enrollment, head)(atomic, validOnly, sinceLastModified)
          case Some(user) => employee(user.id, head)(atomic, validOnly, sinceLastModified)
          case None => Future.failed(new Throwable(s"no user found for $systemId"))
        }
      case _ =>
        Future.failed(new Throwable(s"none or more than one semester was found, which is marked as the current semester"))
    }
  }

  private def student(student: UUID, enrollment: UUID, semester: Semester)(atomic: Boolean, validOnly: Boolean, sinceLastModified: Option[String]) = for {
    labworks <- labworkDao.filter(l => l.semester === semester.id && l.degree === enrollment, atomic, validOnly, sinceLastModified)
    labworIds = labworks.map(_.id)

    apps <- labworkApplicationDao.filter(app => app.applicant === student && app.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
    groups <- groupDao.filter(g => g.contains(student) && g.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
    cards <- reportCardEntryDao.filter(e => e.student === student && e.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
    evals <- reportCardEvaluationDao.filter(_.student === student, atomic, validOnly, sinceLastModified)
    evalPatterns <- reportCardEvaluationPatternDao.filter(_.labwork.inSet(labworIds), atomic, validOnly, sinceLastModified)
  } yield StudentDashboard(semester, labworks, apps, groups, cards, evals, evalPatterns)

  private def employee(employee: UUID, semester: Semester)(atomic: Boolean, validOnly: Boolean, sinceLastModified: Option[String]) = for {
    auths <- authorityDao.get(List(AuthorityUserFilter(employee.toString)), atomic = true, validOnly, sinceLastModified) // must be atomic
    courses = auths.filter(_.isInstanceOf[AuthorityAtom]).map(_.asInstanceOf[AuthorityAtom]).flatMap(_.course)

    scheduleEntryFilter = List(ScheduleEntrySupervisorFilter(employee.toString), ScheduleEntrySinceFilter(semester.start.stringMillis), ScheduleEntryUntilFilter(semester.end.stringMillis))
    scheduleEntries <- scheduleEntryDao.get(scheduleEntryFilter, atomic, validOnly, sinceLastModified)
  } yield EmployeeDashboard(semester, courses, scheduleEntries)

  protected def db: PostgresProfile.backend.Database

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
  val db: PostgresProfile.backend.Database,
  val userDao: UserDao,
  val semesterDao: SemesterDao,
  val labworkDao: LabworkDao,
  val labworkApplicationDao: LabworkApplicationDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao,
  val reportCardEvaluationPatternDao: ReportCardEvaluationPatternDao,
  val groupDao: GroupDao,
  val authorityDao: AuthorityDao,
  val scheduleEntryDao: ScheduleEntryDao
) extends DashboardDao