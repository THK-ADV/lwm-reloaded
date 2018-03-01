package modules

import java.io.File

import services._
import services.backup.{BackupServiceActor, PSQLBackupService}

import scala.util.Try

trait CronServiceModule {
  self: ConfigurationModule =>

  def cronService: CronService
}

trait DefaultCronServiceModule extends CronServiceModule {
  self: AkkaActorSystemModule with ConfigurationModule with LdapModule with UserDaoModule with CourseDaoModule with LabworkDaoModule =>

  val cronService = {
    val backupCron = lwmConfig.getString("lwm.backup.path")
      .filter(_.nonEmpty)
      .flatMap(p => Try(new File(p)).toOption)
      .map { f => f.mkdirs; f }
      .map { f =>
        CronJob(
          lwmConfig.getString("lwm.backup.cron").filter(_.nonEmpty) getOrElse "0 0 4 1/1 * ? *", // every day at 04:00 am
          system.actorOf(BackupServiceActor.props(new PSQLBackupService(courseDao, labworkDao), f)),
          BackupServiceActor.BackupRequestAsync
        )
      }

    val syncCron = CronJob(
      lwmConfig.getString("lwm.ldap.cron").filter(_.nonEmpty) getOrElse "0 0 3 1/1 * ? *", // every day at 03:00 am
      system.actorOf(LdapSyncServiceActor.props(ldapService, userDao)),
      LdapSyncServiceActor.SyncRequest
    )

    new ActorBasedCronService(system, backupCron.fold(List(syncCron))(cj => List(syncCron, cj)))
  }
}
