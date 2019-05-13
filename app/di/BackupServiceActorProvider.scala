package di

import java.io.File

import akka.actor.{ActorRef, ActorSystem}
import javax.inject.{Inject, Provider, Singleton}
import play.api.Configuration
import service.actor.BackupServiceActor
import service.backup.BackupService

import scala.util.Try

@Singleton
class BackupServiceActorProvider @Inject()(
  private val system: ActorSystem,
  private val backupService: BackupService,
  private implicit val config: Configuration
) extends Provider[ActorRef] with ConfigReader {

  lazy val get = {
    val file = for {
      filePath <- nonEmptyConfig("lwm.backup.path")
      file <- Try(new File(filePath)).toOption
      _ <- Try(file.mkdirs).toOption
    } yield file

    /* throwing providers seems to be complicated to support.
    thus, optionality has to be handled internally.
    see https://github.com/google/guice/wiki/ThrowingProviders */

    system.actorOf(BackupServiceActor.props(backupService, file))
  }
}
