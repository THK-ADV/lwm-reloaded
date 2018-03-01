package services

import java.io.File

import akka.util.Timeout
import base.TestBaseDefinition
import dao.{CourseDao, LabworkDao}
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
  import scala.concurrent.duration._
  import models.PostgresCourse.{writes => cw}
  import models.PostgresLabwork.{writes => lw}

  private implicit val encoding: String = BackupService.encoding
  private implicit val timeout: FiniteDuration = Timeout(5.seconds).duration
  private implicit def listWrites[W](implicit w: Writes[W]): Writes[List[W]] = Writes.list[W](w)

  private val courseDao = mock[CourseDao]
  private val labworkDao = mock[LabworkDao]
  private val backupService = new PSQLBackupService(courseDao, labworkDao)

  val c = courses.map(_.toLwmModel)
  val l = labworks.map(_.toLwmModel)
  val jsonList = List(Json.toJson(c), Json.toJson(l))

  private val destFolder = {
    val file = new File("test", "test_dir")
    Try(file).map(_.mkdirs).map(_ => file)
  }

  "A BackupService" should {

    "return json based backup items from data source" in {
      val result = backupItems

      result.size shouldBe jsonList.size
      jsonList.zipWithIndex.forall {
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

  private def assertBackup(result: Vector[File]): Unit = {
    result.size shouldBe jsonList.size
    result.map(f => FileUtils.readFileToString(f, encoding)).zipWithIndex.forall {
      case (jsonString, index) => jsonList(index) == Json.parse(jsonString)
    } shouldBe true
  }

  private def backupItems: Vector[BackupItem[JsValue]] = {
    when(courseDao.get()).thenReturn(Future.successful(c))
    when(labworkDao.get()).thenReturn(Future.successful(l))

    Await.result(backupService.backupItems, timeout)
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    destFolder.isSuccess shouldBe true
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    FileUtils.deleteDirectory(destFolder.get)
  }
}
