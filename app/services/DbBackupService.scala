package services

import java.io.File

import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import org.apache.commons.io.FileUtils
import org.joda.time.LocalDateTime
import services.BackupServiceActor.BackupRequest
import us.theatr.akka.quartz._

import scala.util.{Failure, Success, Try}

trait DbBackupService {
  val cronExpression: String
  val storeFolder: Option[File]
  val backupFolder: Option[File]
}

class ActorBasedBackupService(val system: ActorSystem, val storeFolder: Option[File], val backupFolder: Option[File], val cronExpression: String) extends DbBackupService {

  storeFolder.zip(backupFolder).foreach {
    case (store, backup) =>
      val quartzActor = system.actorOf(Props[QuartzActor])
      val destinationActorRef = system.actorOf(BackupServiceActor.props(store, backup))

      quartzActor ! AddCronSchedule(destinationActorRef, cronExpression, BackupRequest)
  }
}

object BackupServiceActor {

  def props(srcFolder: File, destFolder: File): Props = Props(new BackupServiceActor(srcFolder, destFolder))

  case object BackupRequest
}

class BackupServiceActor(srcFolder: File, destFolder: File) extends Actor with ActorLogging {

  override def receive: Receive = {
    case BackupRequest =>
      srcFolder.listFiles.find(_.getName.endsWith(".data")).map { memstore =>
        val dest = new File(destFolder, s"${srcFolder.getName}_${LocalDateTime.now.toString("yyyy-MM-dd_HH:mm")}")
        Try(FileUtils.copyFileToDirectory(memstore, dest)).map(_ => dest)
      } match {
        case Some(Success(file)) => log.info(s"backup succeeded ${file.getAbsolutePath}")
        case Some(Failure(e)) => log.error("Oops, db backup failed", e.getMessage)
        case None => log.info("Oops, could not find data to back up")
      }
  }
}