package service

import dao._
import dao.helper.TableFilter
import database.ReportCardEntryTable
import database.helper.LdapUserStatus
import models._
import slick.jdbc.JdbcBackend.Database
import slick.jdbc.JdbcProfile
import utils.Ops.unwrap

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

trait StudentSearchService {
  type StudentSearchDashboard = (User, Semester, Map[CourseAtom, Seq[(LabworkAtom, Seq[(ReportCardEntryLike, Set[AnnotationLike])], Seq[ReportCardEvaluationLike])]])

  def semesterDao: SemesterDao

  def userDao: UserDao

  def reportCardEntryDao: ReportCardEntryDao

  def authorityDao: AuthorityDao

  def annotationDao: AnnotationDao

  def labworkDao: LabworkDao

  def evaluationDao: ReportCardEvaluationDao

  def dashboard(currentUserSystemId: String, studentId: UUID): Future[StudentSearchDashboard]
}

final class StudentSearchServiceImpl @Inject()(
  val semesterDao: SemesterDao,
  val userDao: UserDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val authorityDao: AuthorityDao,
  val annotationDao: AnnotationDao,
  val labworkDao: LabworkDao,
  val evaluationDao: ReportCardEvaluationDao,
  val profile: JdbcProfile,
  val db: Database,
  implicit val ctx: ExecutionContext
) extends StudentSearchService {

  import TableFilter.userFilter
  import profile.api._

  def dashboard(currentUserSystemId: String, studentId: UUID): Future[StudentSearchDashboard] = for {
    currentUser <- currentUser(currentUserSystemId)
    student <- student(studentId)
    semester <- currentSemester()
    auths <- authorities(currentUser.id) if auths.isEmpty || auths.get.nonEmpty // user has to be either a admin or have at least one course related authority
    reportCardEntries <- reportCardEntries(studentId, auths, semester.id, currentUser.status)
    groupedReportCardEntries = reportCardEntries.groupBy(_._1.labworkId)
    labworkIds = reportCardEntries.map(_._1.labworkId).distinct
    labworks <- labworks(labworkIds)
    evals <- evaluations(student.id, labworkIds)
    groupedEvals = evals.groupBy(_.labworkId)
    results = labworks.groupBy(_.course).map {
      case (course, labworks) => (
        course,
        labworks
          .sortWith((a, b) => a.semester.start.isAfter(b.semester.start))
          .map { l =>
            (l, groupedReportCardEntries(l.id).sortBy(a => a._1.assignmentIndex), groupedEvals.getOrElse(l.id, Nil))
          }
      )
    }
  } yield (student, semester, results)

  private def evaluations(student: UUID, labworks: Seq[UUID]): Future[Seq[ReportCardEvaluationLike]] =
    evaluationDao.filter(e => userFilter(student).apply(e) && e.labwork.inSet(labworks), atomic = false)

  private def currentUser(systemId: String): Future[User] =
    unwrap(userDao.getBySystemId(systemId, atomic = false), () => s"none or more than one user found for systemId $systemId")

  private def currentSemester(): Future[Semester] =
    unwrap(semesterDao.current, () => s"none or more than one semester was found, but there should only one current semester")

  private def student(id: UUID): Future[User] =
    unwrap(userDao.getSingle(id), () => s"none or more than one user found for id $id")

  private def authorities(userId: UUID): Future[Option[Seq[UUID]]] = for {
    isAdmin <- authorityDao.isAdmin(userId)
    auths <- if (isAdmin)
      Future.successful(None)
    else {
      val query = authorityDao
        .filterValidOnly(a => userFilter(userId).apply(a) && a.course.isDefined)
        .map(_.course.get)
      db.run(query.result).map(Some.apply)
    }
  } yield auths

  private def reportCardEntries(
    student: UUID,
    courses: Option[Seq[UUID]],
    currentSemester: UUID,
    currentUserStatus: LdapUserStatus
  ): Future[Seq[(ReportCardEntryLike, Set[AnnotationLike])]] = {
    def byStudent: ReportCardEntryTable => Rep[Boolean] = r => r.user === student

    def inCourse(courses: Seq[UUID]): ReportCardEntryTable => Rep[Boolean] = r => r.memberOfCourses(courses)

    def inSemester(): ReportCardEntryTable => Rep[Boolean] = r => r.inSemester(currentSemester)

    val filter = courses match {
      case Some(courses) =>
        currentUserStatus match {
          case LdapUserStatus.EmployeeStatus | LdapUserStatus.LecturerStatus =>
            List(byStudent, inCourse(courses)) // course related employee of some kind
          case LdapUserStatus.StudentStatus =>
            List(byStudent, inCourse(courses), inSemester()) // course related student
        }
      case None =>
        List(byStudent) // admin
    }

    val query = reportCardEntryDao.filterBy(filter)
    reportCardEntryDao.withAnnotations(query, atomic = true)
  }

  private def labworks(ids: Seq[UUID]): Future[Seq[LabworkAtom]] =
    labworkDao.getMany(ids.toList).map(_.map(_.asInstanceOf[LabworkAtom]))
}
