package service

import java.io.File

import base.DatabaseSpec
import dao._
import models.UniqueEntity
import org.apache.commons.io.FileUtils
import org.mockito.Mockito._
import org.scalatest.TryValues
import org.scalatest.mockito.MockitoSugar
import play.api.inject.guice.GuiceableModule
import play.api.libs.json.{Json, Writes}
import service.backup.{BackupItem, BackupService, PSQLBackupService}

import scala.concurrent.Future
import scala.util.Try

final class BackupServiceSpec extends DatabaseSpec with MockitoSugar with TryValues {

  import dao.AbstractDaoSpec._

  import scala.concurrent.ExecutionContext.Implicits.global

  private implicit val encoding: String = BackupService.encoding

  private implicit def listWrites[W](implicit w: Writes[W]): Writes[List[W]] = Writes.list[W](w)

  val usr = (employees ++ students).map(_.toUniqueEntity)
  val crs = courses.map(_.toUniqueEntity)
  val deg = degrees.map(_.toUniqueEntity)
  val lapps = labworkApplications.map(_.toUniqueEntity)
  val labs = labworks.map(_.toUniqueEntity)
  val rms = rooms.map(_.toUniqueEntity)
  val sems = semesters.map(_.toUniqueEntity)
  val cards = reportCardEntries.map(_.toUniqueEntity)
  val auths = authorities.map(_.toUniqueEntity)
  val schedules = scheduleEntries.map(_.toUniqueEntity)
  val grps = groups.map(_.toUniqueEntity)


  private val userDao = mock[UserDao]
  private val assignmentPlanDao = mock[AssignmentPlanDao]
  private val courseDao = mock[CourseDao]
  private val degreeDao = mock[DegreeDao]
  private val labworkApplicationDao = mock[LabworkApplicationDao]
  private val labworkDao = mock[LabworkDao]
  private val roleDao = mock[RoleDao]
  private val roomDao = mock[RoomDao]
  private val semesterDao = mock[SemesterDao]
  private val timetableDao = mock[TimetableDao]
  private val blacklistDao = mock[BlacklistDao]
  private val reportCardEntryDao = mock[ReportCardEntryDao]
  private val authorityDao = mock[AuthorityDao]
  private val scheduleEntryDao = mock[ScheduleEntryDao]
  private val groupDao = mock[GroupDao]
  private val reportCardEvaluationDao = mock[ReportCardEvaluationDao]

  private val backupService = new PSQLBackupService(userDao, assignmentPlanDao, courseDao, degreeDao, labworkApplicationDao,
    labworkDao, roleDao, roomDao, semesterDao, timetableDao, blacklistDao, reportCardEntryDao,
    authorityDao, scheduleEntryDao, groupDao, reportCardEvaluationDao)

  lazy val jsonSeq = Seq(
    toJson(usr),
    toJson(crs),
    toJson(deg),
    toJson(lapps),
    toJson(labs),
    toJson(rms),
    toJson(sems),
    toJson(cards),
    toJson(auths),
    toJson(schedules),
    toJson(grps)
  )

  def toJson(seq: Seq[UniqueEntity]) = backupService.toJson(seq)

  private val destFolder = {
    val file = new File("test", "test_dir")
    Try(file).map(_.mkdirs).map(_ => file)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    destFolder.isSuccess shouldBe true
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    FileUtils.deleteDirectory(destFolder.get)
  }

  "A BackupService" should {

    "return json based backup items from data source" in {
      async(backupItems) { result =>
        result.size shouldBe jsonSeq.size
        result.map(_.data) should contain theSameElementsAs jsonSeq
      }
    }

    "write backup items to given location" in {
      async(backupItems) { items =>
        val result = backupService.persist(items, destFolder.get, shouldOverride = false)
        assertBackup(result.success.value)
      }
    }

    "backup items from data source and write them to given location" in {
      async(backupService.backup(destFolder.get, shouldOverride = true))(assertBackup)
    }

    "restore backup" in {} // TODO
  }

  private def assertBackup(files: Vector[File]): Unit = {
    files.size shouldBe jsonSeq.size

    val jsonFiles = files.map(f => Json.parse(FileUtils.readFileToString(f, encoding)))
    jsonFiles should contain theSameElementsAs jsonSeq
  }

  private def backupItems: Future[Vector[BackupItem]] = {
    when(userDao.get()).thenReturn(Future.successful(usr))
    when(assignmentPlanDao.get()).thenReturn(Future.successful(Seq.empty))
    when(courseDao.get()).thenReturn(Future.successful(crs))
    when(degreeDao.get()).thenReturn(Future.successful(deg))
    when(labworkApplicationDao.get()).thenReturn(Future.successful(lapps))
    when(labworkDao.get()).thenReturn(Future.successful(labs))
    when(roleDao.get()).thenReturn(Future.successful(Seq.empty))
    when(roomDao.get()).thenReturn(Future.successful(rms))
    when(semesterDao.get()).thenReturn(Future.successful(sems))
    when(timetableDao.get()).thenReturn(Future.successful(Seq.empty))
    when(blacklistDao.get()).thenReturn(Future.successful(Seq.empty))
    when(reportCardEntryDao.get()).thenReturn(Future.successful(cards))
    when(authorityDao.get()).thenReturn(Future.successful(auths))
    when(scheduleEntryDao.get()).thenReturn(Future.successful(schedules))
    when(groupDao.get()).thenReturn(Future.successful(grps))
    when(reportCardEvaluationDao.get()).thenReturn(Future.successful(Seq.empty))

    backupService.backupItems
  }

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
