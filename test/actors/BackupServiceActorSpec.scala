package actors

/*
import java.io.File
import java.time.LocalTime
import java.util.UUID

import akka.actor.ActorSystem
import akka.pattern.ask
import akka.testkit.TestKit
import akka.util.Timeout
import base.TestBaseDefinition
import org.apache.commons.io.FileUtils
import org.mockito.Mockito._
import org.scalatest.WordSpecLike
import org.scalatest.mock.MockitoSugar.mock
import services.backup.BackupServiceActor.{BackupResult, Failed, Succeeded}
import services.backup.{BackupService, BackupServiceActor, StringBackupItem}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.util.Try

final class BackupServiceActorSpec extends TestKit(ActorSystem("BackupServiceActorSpec")) with WordSpecLike with TestBaseDefinition {

  implicit val timeout: Timeout = Timeout(5.seconds)

  val destFolder = {
    val file = new File("test", "test_dir")
    Try(file).map(_.mkdirs).map(_ => file)
  }

  val backupService = mock[BackupService[String]]
  val ref = system.actorOf(BackupServiceActor.props(backupService, destFolder.get))

  "A Backup Service Actor" should {

    "perform a backup by writing all data into a file" in {
      val strings = (0 until 10).map(i => StringBackupItem("String", i.toString)).toVector
      val files = strings.map(s => new File(destFolder.get, s"${UUID.randomUUID}_${s.fileName}.${s.fileExtension}"))

      when(backupService.backup(destFolder.get, shouldOverride = false)).thenReturn(Future.successful(files))

      val f = (ref ? BackupServiceActor.BackupRequestSync).mapTo[BackupResult]

      Await.result(f, timeout.duration) match {
        case Succeeded(res) =>
          res shouldBe files
        case Failed(t) =>
          fail(s"backup-service should backup ${strings.size} items", t)
      }
    }

    "calculate delay based on given interval" in {
      import scala.concurrent.duration._

      val desired = LocalTime.of(12, 0, 0)
      val now = LocalTime.of(8, 0, 0)

      val d1 = BackupServiceActor.delayUntil(desired, now)
      (12 - 8).hours.toSeconds shouldBe d1.toSeconds

      val d2 = BackupServiceActor.delayUntil(desired, now.plusHours(4))
      d2.toSeconds shouldBe 0L

      val future = now.plusHours(4).plusMinutes(30)
      val d3 = BackupServiceActor.delayUntil(desired, future)
      d3.toSeconds shouldBe (24.hour - 30.minutes).toSeconds
    }
  }

  override protected def beforeAll(): Unit = {
    super.beforeAll()

    destFolder.isSuccess shouldBe true
  }

  override protected def afterAll(): Unit = {
    super.afterAll()

    system.terminate()
    FileUtils.deleteDirectory(destFolder.get)
  }
}
*/
