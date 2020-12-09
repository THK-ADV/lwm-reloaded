package service.dashboard

import dao.{DashboardDao, SemesterDao, UserDao}
import javax.inject.Inject
import models._
import utils.Ops.unwrap

import scala.concurrent.{ExecutionContext, Future}

trait DashboardService {
  def dao: DashboardDao

  def semesterDao: SemesterDao

  def userDao: UserDao

  def dashboard(systemId: String, config: DashboardConfig): Future[Dashboard]
}

final class DashboardServiceImpl @Inject()(
  val dao: DashboardDao,
  val semesterDao: SemesterDao,
  val userDao: UserDao,
  implicit val ctx: ExecutionContext
) extends DashboardService {

  override def dashboard(systemId: String, config: DashboardConfig): Future[Dashboard] = for {
    semester <- currentSemester(config.atomic)
    user <- currentUser(systemId, config.atomic)
    board <- user match {
      case employee: Employee =>
        employeeDashboard(employee, semester, config)
      case lecturer: Lecturer =>
        employeeDashboard(lecturer, semester, config)
      case student: StudentLike =>
        studentDashboard(student, semester, config)
      case _ =>
        Future.failed(new Throwable(s"unknown user type $user"))
    }
  } yield board

  private def employeeDashboard(
    employee: User,
    semester: Semester,
    config: DashboardConfig
  ): Future[EmployeeDashboard] =
    dao.employeeDashboard(
      employee,
      semester,
      config.atomic,
      config.numberOfUpcomingElements,
      config.entriesSinceNow getOrElse true,
      config.sortedByDate getOrElse true,
      config.ownEntriesOnly getOrElse true
    )

  private def studentDashboard(
    student: StudentLike,
    semester: Semester,
    config: DashboardConfig
  ): Future[StudentDashboard] =
    dao.studentDashboard(
      student,
      semester,
      config.atomic,
      config.numberOfUpcomingElements,
      config.entriesSinceNow getOrElse true,
      config.sortedByDate getOrElse true,
      config.ownEntriesOnly getOrElse true
    )

  private def currentUser(systemId: String, atomic: Boolean) =
    unwrap(userDao.getBySystemId(systemId, atomic), () => s"none or more than one user found for systemId $systemId")

  private def currentSemester(atomic: Boolean): Future[Semester] =
    unwrap(semesterDao.current(atomic), () => s"none or more than one semester was found, but there should only one current semester")
}
