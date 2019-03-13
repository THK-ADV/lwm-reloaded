package services.backup

import java.io.File

import akka.actor.{Actor, ActorLogging}
import javax.inject.Inject
import services.backup.BackupServiceActor.{BackupRequestAsync, BackupRequestSync, Failed, Succeeded}

import scala.concurrent.ExecutionContextExecutor
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

      backup(
        files => requester ! Succeeded(files),
        error => requester ! Failed(error)
      )
    case BackupRequestAsync =>
      log.info("beginning backup request")

      backup(_ => Unit, _ => Unit)
  }

  private def backup(success: Vector[File] => Unit, failure: Throwable => Unit): Unit = {
    destFolder match {
      case Some(f) =>
        backupService.backup(f, shouldOverride = false).onComplete {
          case Success(files) =>
            log.info(s"successfully backup'ed ${files.size} files")
            success(files)
          case Failure(error) =>
            log.error(s"failed backup request with exception: ${error.getLocalizedMessage}")
            failure(error)
        }
      case None =>
        log.error(s"no folder to back up")
    }
  }
}
