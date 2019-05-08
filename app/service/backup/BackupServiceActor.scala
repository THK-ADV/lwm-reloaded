package service.backup

import java.io.File

import akka.actor.{Actor, ActorLogging}
import javax.inject.Inject
import service.backup.BackupServiceActor.{BackupRequestAsync, BackupRequestSync, Failed, Succeeded}

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object BackupServiceActor {

  sealed trait BackupResult

  case class Failed(throwable: Throwable) extends BackupResult

  case class Succeeded(files: Vector[File]) extends BackupResult

  case object BackupRequestSync

  case object BackupRequestAsync

}

final class BackupServiceActor @Inject()(backupService: BackupService, destFolder: Option[File]) extends Actor with ActorLogging {

  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  override def receive = {
    case BackupRequestSync =>
      log.info("beginning backup request")

      val requester = sender()

      backup onComplete {
        case Success(files) =>
          log.info(s"successfully backup'ed ${files.size} files")
          requester ! Succeeded(files)
        case Failure(e) =>
          log.error(e.getLocalizedMessage)
          requester ! Failed(e)
      }
    case BackupRequestAsync =>
      log.info("beginning backup request")

      backup()
  }

  private def backup(): Future[Vector[File]] = destFolder match {
    case Some(f) =>
      backupService.backup(f, shouldOverride = false)
    case None =>
      Future.failed(new Throwable("no folder to back up"))
  }
}
