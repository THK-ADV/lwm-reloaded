package service.actor

import java.io.File

import akka.actor.{Actor, ActorLogging, Props}
import javax.inject.{Inject, Singleton}
import service.backup.BackupService

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.{Failure, Success}

object BackupServiceActor {

  def props(backupService: BackupService, destFolder: Option[File]) =
    Props(new BackupServiceActor(backupService, destFolder))

  case object BackupRequestSync

  case object BackupRequestAsync

}

@Singleton
final class BackupServiceActor @Inject()(
  private val backupService: BackupService,
  private val destFolder: Option[File]
) extends Actor with ActorLogging {

  import BackupServiceActor._

  private implicit val dispatcher: ExecutionContextExecutor = context.system.dispatcher

  override def receive = {
    case BackupRequestSync =>
      log.info("beginning backup request")

      val requester = sender()

      backup onComplete {
        case Success(files) => requester ! files
        case Failure(error) => requester ! error
      }
    case BackupRequestAsync =>
      log.info("beginning backup request")

      backup onComplete {
        case Success(files) =>
          log.info(s"successfully backup'ed ${files.size} files")
        case Failure(throwable) =>
          log.error(s"failed to backup with throwable: ${throwable.getLocalizedMessage}")
      }
  }

  private def backup(): Future[Vector[File]] = destFolder match {
    case Some(f) => backupService.backup(f, shouldOverride = false)
    case None => Future.failed(new Throwable("no folder to back up"))
  }
}
