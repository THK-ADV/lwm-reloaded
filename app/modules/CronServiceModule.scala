package modules

import java.io.File

import akka.actor.ActorRef
import services._
import services.backup.{BackupServiceActor, PSQLBackupService}
import services.blacklist.BlacklistServiceActor
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
    with GroupDaoManagementModule with SessionRepositoryModule with ReportCardEvaluationDaoModule with BlacklistServiceModule =>

  val cronService = {
    val backupCron = for {
      filePath <- config("lwm.backup.path")
      expression <- config("lwm.backup.cron")
      file <- Try(new File(filePath)).toOption
      _ <- Try(file.mkdirs).toOption
    } yield CronJob(
      expression,
      system.actorOf(BackupServiceActor.props(
        new PSQLBackupService(userDao, assignmentPlanDao, courseDao, degreeDao, labworkApplicationDao,
          labworkDao, roleDao, roomDao, semesterDao, timetableDao, blacklistDao, reportCardEntryDao,
          authorityDao, scheduleEntryDao, groupDao, reportCardEvaluationDao), file)),
      BackupServiceActor.BackupRequestAsync
    )

    val syncCron = cronJob(
      "lwm.ldap.cron",
      system.actorOf(LdapSyncServiceActor.props(ldapService, userDao)),
      LdapSyncServiceActor.SyncRequest
    )

    val blacklistCron = for {
      year <- config("lwm.blacklist.year")
      expression <- config("lwm.blacklist.cron")
      describableYear <- NaturalDescribableYear(year)
    } yield CronJob(
      expression,
      system.actorOf(BlacklistServiceActor.props(blacklistService, blacklistDao, describableYear)),
      BlacklistServiceActor.BlacklistDownloadRequest
    )

    val semesterCron = for {
      year <- config("lwm.semester.year")
      expression <- config("lwm.semester.cron")
      describableYear <- NaturalDescribableYear(year)
    } yield CronJob(
      expression,
      system.actorOf(SemesterCreationActor.props(semesterDao, describableYear)),
      SemesterCreationActor.CreationRequest
    )

    val cronJobs = List(backupCron, syncCron, blacklistCron, semesterCron).filter(_.isDefined).map(_.get)
    new ActorBasedCronService(system, cronJobs)
  }

  def cronJob(configExpressionString: String, actorRef: ActorRef, message: Any): Option[CronJob] = config(configExpressionString).map(exp => CronJob(exp, actorRef, message))

  def config(string: String): Option[String] = lwmConfig.getString(string).filter(_.nonEmpty)
}
