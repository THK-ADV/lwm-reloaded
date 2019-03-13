package di

import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import javax.inject.{Inject, Provider, Singleton}
import play.api.Configuration
import services.backup.{BackupService, BackupServiceActor}

import scala.util.Try

@Singleton
class BackupServiceProvider @Inject()(system: ActorSystem, backupService: BackupService, config: Configuration) extends Provider[ActorRef] {

  lazy val get = {
    def nonEmptyConfig(name: String): Option[String] = (config getOptional[String] name) filter (_.nonEmpty)

    val file = for {
      filePath <- nonEmptyConfig("lwm.backup.path")
      file <- Try(new File(filePath)).toOption
      _ <- Try(file.mkdirs).toOption
    } yield file

    /* throwing providers seems to be complicated to support.
    thus, optionality has to be handled internally.
    see https://github.com/google/guice/wiki/ThrowingProviders */

    system.actorOf(Props(new BackupServiceActor(backupService, file)))
  }
}
