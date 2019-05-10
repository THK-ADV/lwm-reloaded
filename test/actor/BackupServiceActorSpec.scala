package actor

import java.io.File

import akka.actor.ActorSystem
import akka.testkit.{ImplicitSender, TestKit}
import base.TestBaseDefinition
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.WordSpecLike
import org.scalatest.mockito.MockitoSugar.mock
import service.actor.BackupServiceActor
import service.backup.BackupService

import scala.concurrent.Future

class BackupServiceActorSpec
  extends TestKit(ActorSystem("BackupServiceActorSpec"))
    with WordSpecLike
    with TestBaseDefinition
    with ImplicitSender {

  import BackupServiceActor._

  val backupService = mock[BackupService]

  override def afterAll: Unit = {
    TestKit.shutdownActorSystem(system)
  }

  "A BackupServiceActorSpec" should {

    "backup files (sync)" in {
      val files = (0 until 5).map(_ => mock[File]).toVector
      when(backupService.backup(any(), any())(any(), any())).thenReturn(Future.successful(files))

      val props = BackupServiceActor.props(backupService, Some(mock[File]))
      val actor = system.actorOf(props)

      actor ! BackupRequestSync

      expectMsg(files)
    }

    "backup files (async)" in {
      val files = (0 until 5).map(_ => mock[File]).toVector
      when(backupService.backup(any(), any())(any(), any())).thenReturn(Future.successful(files))

      val props = BackupServiceActor.props(backupService, Some(mock[File]))
      val actor = system.actorOf(props)

      actor ! BackupRequestAsync

      expectNoMessage()
    }

    "fail when dest folder can't be created" in {
      when(backupService.backup(any(), any())(any(), any())).thenReturn(Future.successful(Vector.empty))

      val props = BackupServiceActor.props(backupService, None)
      val actor = system.actorOf(props)

      actor ! BackupRequestSync

      expectMsgClass(classOf[Throwable])
    }

    "fail when backupService failes" in {
      when(backupService.backup(any(), any())(any(), any())).thenReturn(Future.failed(new Throwable("failed")))

      val props = BackupServiceActor.props(backupService, Some(mock[File]))
      val actor = system.actorOf(props)

      actor ! BackupRequestSync

      expectMsgClass(classOf[Throwable])
    }
  }
}