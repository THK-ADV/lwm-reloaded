package service

/*import java.io.File

import akka.util.Timeout
import base.TestBaseDefinition
import dao._
import org.apache.commons.io.FileUtils
import org.mockito.Mockito._
import org.scalatest.WordSpec
import org.scalatest.mock.MockitoSugar.mock
import play.api.libs.json.{JsValue, Json, Writes}
import services.backup.{BackupItem, BackupService, PSQLBackupService}

import scala.concurrent.{Await, Future}
import scala.util.{Failure, Success, Try}

final class BackupServiceSpec extends WordSpec with TestBaseDefinition {

  import dao.AbstractDaoSpec._
  import models.UniqueEntity.toJson
  import scala.concurrent.duration._

  private implicit val encoding: String = BackupService.encoding
  private implicit val timeout: FiniteDuration = Timeout(5.seconds).duration

  private implicit def listWrites[W](implicit w: Writes[W]): Writes[List[W]] = Writes.list[W](w)

  val usr = (employees ++ students).map(_.toUniqueEntity)
  val aps = assignmentPlans.map(_.toUniqueEntity)
  val crs = courses.map(_.toUniqueEntity)
  val deg = degrees.map(_.toUniqueEntity)
  val lapps = labworkApplications.map(_.toUniqueEntity)
  val labs = labworks.map(_.toUniqueEntity)
  val rls = roles.map(_.toUniqueEntity)
  val rms = rooms.map(_.toUniqueEntity)
  val sems = semesters.map(_.toUniqueEntity)
  val tts = timetables.map(_.toUniqueEntity)
  val bls = blacklists.map(_.toUniqueEntity)
  val cards = reportCardEntries.map(_.toUniqueEntity)
  val auths = authorities.map(_.toUniqueEntity)
  val schedules = scheduleEntries.map(_.toUniqueEntity)
  val grps = groups.map(_.toUniqueEntity)
  val evals = reportCardEvaluations.map(_.toUniqueEntity)

  val jsonSeq = toJson(usr, aps, crs, deg, lapps, labs, rls, rms, sems, tts, bls, cards, auths, schedules, grps, evals)

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

  private val destFolder = {
    val file = new File("test", "test_dir")
    Try(file).map(_.mkdirs).map(_ => file)
  }

  "A BackupService" should {

    "return json based backup items from data source" in {
      val result = backupItems

      result.size shouldBe jsonSeq.size
      jsonSeq.zipWithIndex.forall {
        case (json, index) => json == result(index).data
      } shouldBe true
    }

    "write backup item to give location" in {
      val items = backupItems
      val result = backupService.persist(items, destFolder.get, shouldOverride = false)

      result match {
        case Success(files) =>
          assertBackup(files)
        case Failure(e) =>
          fail(s"backup-service should write to ${destFolder.get.getPath}, but failed with exception ${e.getLocalizedMessage}")
      }
    }

    "backup json based backup items from data source and write them to given location" in {
      val result = Await.result(backupService.backup(destFolder.get, shouldOverride = true), timeout)
      assertBackup(result)
    }

    "restore backup" in {} // TODO
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    destFolder.isSuccess shouldBe true
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    FileUtils.deleteDirectory(destFolder.get)
  }

  private def assertBackup(result: Vector[File]): Unit = {
    result.size shouldBe jsonSeq.size
    result.map(f => FileUtils.readFileToString(f, encoding)).zipWithIndex.forall {
      case (jsonString, index) => jsonSeq(index) == Json.parse(jsonString)
    } shouldBe true
  }

  private def backupItems: Vector[BackupItem[JsValue]] = {
    when(userDao.get()).thenReturn(Future.successful(usr))
    when(assignmentPlanDao.get()).thenReturn(Future.successful(aps))
    when(courseDao.get()).thenReturn(Future.successful(crs))
    when(degreeDao.get()).thenReturn(Future.successful(deg))
    when(labworkApplicationDao.get()).thenReturn(Future.successful(lapps))
    when(labworkDao.get()).thenReturn(Future.successful(labs))
    when(roleDao.get()).thenReturn(Future.successful(rls))
    when(roomDao.get()).thenReturn(Future.successful(rms))
    when(semesterDao.get()).thenReturn(Future.successful(sems))
    when(timetableDao.get()).thenReturn(Future.successful(tts))
    when(blacklistDao.get()).thenReturn(Future.successful(bls))
    when(reportCardEntryDao.get()).thenReturn(Future.successful(cards))
    when(authorityDao.get()).thenReturn(Future.successful(auths))
    when(scheduleEntryDao.get()).thenReturn(Future.successful(schedules))
    when(groupDao.get()).thenReturn(Future.successful(grps))
    when(reportCardEvaluationDao.get()).thenReturn(Future.successful(evals))

    Await.result(backupService.backupItems, timeout)
  }
}*/
