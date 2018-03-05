package modules

import java.io.File

import services._
import services.backup.{BackupServiceActor, PSQLBackupService}
import services.ldap.LdapSyncServiceActor

import scala.util.Try

trait CronServiceModule {
  self: ConfigurationModule =>

  def cronService: CronService
}

trait DefaultCronServiceModule extends CronServiceModule {
  self: AkkaActorSystemModule with ConfigurationModule with LdapModule with UserDaoModule with AssignmentPlanDaoModule with CourseDaoModule
    with DegreeDaoModule with LabworkApplicationDaoModule with LabworkDaoModule with RoleDaoModule with RoomDaoModule with SemesterDaoModule
    with TimetableDaoManagementModule with BlacklistDaoManagementModule with ReportCardEntryDaoModule with AuthorityDaoModule with ScheduleEntryDaoModule
    with GroupDaoManagementModule with SessionRepositoryModule with ReportCardEvaluationDaoModule =>

  val cronService = {
    val backupCron = (lwmConfig.getString("lwm.backup.path"), lwmConfig.getString("lwm.backup.cron")) match {
      case (Some(filePath), Some(cronExpression)) if filePath.nonEmpty && cronExpression.nonEmpty =>
        val cronJob = for {
          file <- Try(new File(filePath))
          created <- Try(file.mkdirs) if created
        } yield CronJob(
          cronExpression,
          system.actorOf(BackupServiceActor.props(
            new PSQLBackupService(userDao, assignmentPlanDao, courseDao, degreeDao, labworkApplicationDao,
              labworkDao, roleDao, roomDao, semesterDao, timetableDao, blacklistDao, reportCardEntryDao,
              authorityDao, scheduleEntryDao, groupDao, reportCardEvaluationDao), file)),
          BackupServiceActor.BackupRequestAsync
        )

        cronJob.toOption
      case _ => None
    }

    val syncCron = lwmConfig.getString("lwm.ldap.cron").filter(_.nonEmpty).map { cronExpression =>
      CronJob(
        cronExpression,
        system.actorOf(LdapSyncServiceActor.props(ldapService, userDao)),
        LdapSyncServiceActor.SyncRequest
      )
    }

    val cronJobs = List(backupCron, syncCron).filter(_.isDefined).map(_.get)

    new ActorBasedCronService(system, cronJobs)
  }
}
