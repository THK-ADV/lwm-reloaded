package actors

import java.io.{File, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}

import akka.actor.ActorSystem
import akka.testkit.TestKit
import base.TestBaseDefinition
import org.joda.time.LocalDateTime
import org.scalatest.WordSpecLike
import services.BackupServiceActor

import scala.util.Try

class DbBackupServiceActorSpec extends TestKit(ActorSystem("test_system")) with WordSpecLike with TestBaseDefinition {

  val srcFolder = "lwm_store"
  val destFolder = "db_backup"

  val folders = (for {
    lwm_store <- Try(Files.createDirectory(new File("test/resources", srcFolder).toPath))
    db_backup <- Try(Files.createDirectory(new File(lwm_store.getParent.toString, destFolder).toPath))
    _ <- Try(Files.createFile(new File(lwm_store.toString, "memstore.data").toPath))
  } yield List(lwm_store, db_backup)).getOrElse(List.empty)

  val actorRef = system.actorOf(BackupServiceActor.props(folders.head.toFile, folders.last.toFile))

  "A DbBackupServiceActorSpec " should {

    "successfully backup given store" in {
      import akka.testkit._

      import scala.concurrent.duration._

      10.seconds.dilated

      (0 until 10) foreach { _ => actorRef ! BackupServiceActor.BackupRequest }

      awaitAssert {
        val backups = folders.last.toFile.listFiles()

        backups should not be empty
        backups.find(_.getName.contains(srcFolder)) match {
          case Some(folder) =>
            folder.getAbsolutePath.contains(srcFolder) shouldBe true
            folder.getAbsolutePath.contains(s"${srcFolder}_${LocalDateTime.now.toString("yyyy-MM-dd_HH:mm")}") shouldBe true
            folder.listFiles().nonEmpty shouldBe true
            folder.listFiles().forall(_.getName.endsWith(".data")) shouldBe true

          case None => fail("there should be a least one backup")
        }
      }
    }
  }

  override protected def beforeAll(): Unit = {
    assert(folders.size == 2, "there should be exactly one src and dest folder")
  }

  override protected def afterAll(): Unit = {
    system.terminate

    folders.foreach { p =>
      Files.walkFileTree(p, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          Files.delete(file)
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(dir: Path, exc: IOException): FileVisitResult = {
          Files.delete(dir)
          FileVisitResult.CONTINUE
        }
      })
    }
  }
}
