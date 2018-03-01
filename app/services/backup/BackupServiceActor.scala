package services.backup

import java.io.File
import java.time.LocalTime

import akka.actor.{Actor, ActorLogging, Props}
import services.backup.BackupServiceActor.{BackupRequestAsync, BackupRequestSync, Failed, Succeeded}

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object BackupServiceActor {
  def props[T](backupService: BackupService[T], destFolder: File): Props = Props(new BackupServiceActor(backupService, destFolder))

  import scala.concurrent.duration._

  val interval: FiniteDuration = 24.hours

  /**
    * calculates delay in seconds between `now` and `fireTime` time concerning `interval` boundaries.
    * `fireTime` and `now` needs to be ISO-8601 time formatted such as "13:37:00".
    * @param fireTime destination time to fire
    * @param now current time for calculation purpose
    * @param interval of schedule
    * @return delay in seconds
    */
  def delayUntil(fireTime: LocalTime, now: LocalTime = LocalTime.now, interval: FiniteDuration = interval): FiniteDuration = {
    val d = fireTime.toSecondOfDay
    val n = now.toSecondOfDay
    val diff = d - n

    val delay = if (diff < 0) interval.toSeconds + diff else diff
    delay.seconds
  }

  sealed trait BackupResult

  case class Failed(throwable: Throwable) extends BackupResult

  case class Succeeded(files: Vector[File]) extends BackupResult

  case object BackupRequestSync
  case object BackupRequestAsync
}

final class BackupServiceActor[T](private val backupService: BackupService[T], private val destFolder: File) extends Actor with ActorLogging {

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

  private def backup(success: (Vector[File]) => Unit, failure: (Throwable) => Unit): Unit = {
    backupService.backup(destFolder, shouldOverride = false).onComplete {
      case Success(files) =>
        log.info(s"successfully backup'ed ${files.size} files")
        success(files)
      case Failure(error) =>
        log.error(s"failed backup-request with exception: ${error.getLocalizedMessage}")
        failure(error)
    }
  }
}
