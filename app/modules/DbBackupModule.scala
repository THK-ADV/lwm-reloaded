package modules

import java.io.File
import java.nio.file.Files

import modules.store.DbFolder
import services.{ActorBasedBackupService, DbBackupService}

trait DbBackupModule {

  def backupService: DbBackupService
}

trait DefaultDbBackupModuleImpl extends DbBackupModule {
  self: AkkaActorSystemModule with DbFolder with ConfigurationModule =>

  val cronExpression = lwmConfig.getString("lwm.store.cron") match {
    case Some(cron) if cron.nonEmpty => cron
    case _ => "0 0 3 1/1 * ? *" // every day at 03:00 am
  }

  val backupFolder = lwmConfig.getString("lwm.store.backup") match {
    case Some(path) if path.nonEmpty =>
      val file = new File(path)

      if (Files.exists(file.toPath))
        Some(file)
      else
        Some(Files.createDirectory(file.toPath).toFile)
    case _ => None
  }

  override val backupService: DbBackupService = new ActorBasedBackupService(system, folder, backupFolder, cronExpression)
}
