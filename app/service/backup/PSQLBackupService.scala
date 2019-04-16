package service.backup

import java.io.File
import java.util.UUID

import dao._
import javax.inject.Inject
import org.apache.commons.io.FileUtils
import org.joda.time.LocalDateTime
import play.api.libs.json.Json

import scala.concurrent.Future
import scala.util.Try

final class PSQLBackupService @Inject()(
  val userDao: UserDao,
  val assignmentPlanDao: AssignmentPlanDao,
  val courseDao: CourseDao,
  val degreeDao: DegreeDao,
  val labworkApplicationDao: LabworkApplicationDao,
  val labworkDao: LabworkDao,
  val roleDao: RoleDao,
  val roomDao: RoomDao,
  val semesterDao: SemesterDao,
  val timetableDao: TimetableDao,
  val blacklistDao: BlacklistDao,
  val reportCardEntryDao: ReportCardEntryDao,
  val authorityDao: AuthorityDao,
  val scheduleEntryDao: ScheduleEntryDao,
  val groupDao: GroupDao,
  val reportCardEvaluationDao: ReportCardEvaluationDao
) extends BackupService {

  import scala.concurrent.ExecutionContext.Implicits.global

  override def backupItems: Future[Vector[BackupItem]] = {
    for {
      users <- userDao.get(atomic = false, validOnly = false)
      assignmentPlans <- assignmentPlanDao.get(atomic = false, validOnly = false)
      courses <- courseDao.get(atomic = false, validOnly = false)
      degrees <- degreeDao.get(atomic = false, validOnly = false)
      labworkApplications <- labworkApplicationDao.get(atomic = false, validOnly = false)
      labworks <- labworkDao.get(atomic = false, validOnly = false)
      roles <- roleDao.get(atomic = false, validOnly = false)
      rooms <- roomDao.get(atomic = false, validOnly = false)
      semesters <- semesterDao.get(atomic = false, validOnly = false)
      timetables <- timetableDao.get(atomic = false, validOnly = false)
      blacklists <- blacklistDao.get(atomic = false, validOnly = false)
      reportCardEntries <- reportCardEntryDao.get(atomic = false, validOnly = false)
      authorities <- authorityDao.get(atomic = false, validOnly = false)
      scheduleEntries <- scheduleEntryDao.get(atomic = false, validOnly = false)
      groups <- groupDao.get(atomic = false, validOnly = false)
      reportCardEvaluations <- reportCardEvaluationDao.get(atomic = false, validOnly = false)
    } yield List(users, assignmentPlans, courses, degrees, labworkApplications, labworks, roles, rooms,
      semesters, timetables, blacklists, reportCardEntries, authorities, scheduleEntries, groups, reportCardEvaluations)
      .filter(_.nonEmpty)
      .map(seq => BackupItem(seq.head.getClass.getSimpleName, Json.toJson(seq)))
      .toVector
  }

  override def persist(items: Vector[BackupItem], rootFolder: File, shouldOverride: Boolean)(implicit encoding: String): Try[Vector[File]] = {
    import utils.Ops.MonadInstances.tryM
    import utils.Ops._

    val backupFile = new File(rootFolder, s"Backup_${LocalDateTime.now.toString("yyyy-MM-dd-HH:mm:ss")}")

    items.map { item =>
      val file = if (shouldOverride)
        new File(backupFile, s"${item.fileName}.${item.fileExtension}")
      else
        new File(backupFile, s"${UUID.randomUUID.hashCode.toString}_${item.fileName}.${item.fileExtension}")

      Try(FileUtils.write(file, item.writable, encoding)).map(_ => file) // side effect
    }.sequence
  }
}