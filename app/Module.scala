import akka.actor.ActorRef
import auth.{KeycloakAuthorization, OAuthAuthorization}
import com.google.inject.AbstractModule
import com.google.inject.name.Names
import dao._
import di._
import javax.inject.Singleton
import org.keycloak.adapters.KeycloakDeployment
import play.api.{Configuration, Environment}
import service.backup.{BackupService, BackupServiceActor, PSQLBackupService}
import service.blacklist.{BlacklistApiService, BlacklistApiServiceImpl}
import service.{ActorScheduler, MailerService, ScheduleService, Webservice}
import slick.jdbc.PostgresProfile.api._

class Module(environment: Environment, config: Configuration) extends AbstractModule {

  override def configure(): Unit = {
    bindDatabase()
    bindDaos()
    bindServices()
    bindActors()
  }

  private def bindServices(): Unit = {
    bind(classOf[Webservice]).in(classOf[Singleton])
    bind(classOf[BlacklistApiService]).to(classOf[BlacklistApiServiceImpl]).in(classOf[Singleton])
    bind(classOf[OAuthAuthorization]).to(classOf[KeycloakAuthorization]).in(classOf[Singleton])
    bind(classOf[BackupService]).to(classOf[PSQLBackupService]).in(classOf[Singleton])

    bind(classOf[ScheduleService]).toProvider(classOf[ScheduleServiceProvider])
    bind(classOf[KeycloakDeployment]).toProvider(classOf[KeycloakDeploymentProvider])
    bind(classOf[MailerService]).toProvider(classOf[MailerServiceProvider])
  }

  private def bindDaos(): Unit = {
    bind(classOf[SemesterDao]).to(classOf[SemesterDaoImpl]).in(classOf[Singleton])
    bind(classOf[AuthorityDao]).to(classOf[AuthorityDaoImpl]).in(classOf[Singleton])
    bind(classOf[RoleDao]).to(classOf[RoleDaoImpl]).in(classOf[Singleton])
    bind(classOf[AssignmentPlanDao]).to(classOf[AssignmentPlanDaoImpl]).in(classOf[Singleton])
    bind(classOf[BlacklistDao]).to(classOf[BlacklistDaoImpl]).in(classOf[Singleton])
    bind(classOf[CourseDao]).to(classOf[CourseDaoImpl]).in(classOf[Singleton])
    bind(classOf[DegreeDao]).to(classOf[DegreeDaoImpl]).in(classOf[Singleton])
    bind(classOf[GroupDao]).to(classOf[GroupDaoImpl]).in(classOf[Singleton])
    bind(classOf[RoomDao]).to(classOf[RoomDaoImpl]).in(classOf[Singleton])
    bind(classOf[LabworkApplicationDao]).to(classOf[LabworkApplicationDaoImpl]).in(classOf[Singleton])
    bind(classOf[LabworkDao]).to(classOf[LabworkDaoImpl]).in(classOf[Singleton])
    bind(classOf[ReportCardEntryDao]).to(classOf[ReportCardEntryDaoImpl]).in(classOf[Singleton])
    bind(classOf[ReportCardEntryTypeDao]).to(classOf[ReportCardEntryTypeDaoImpl]).in(classOf[Singleton])
    bind(classOf[ReportCardEvaluationDao]).to(classOf[ReportCardEvaluationDaoImpl]).in(classOf[Singleton])
    bind(classOf[ReportCardEvaluationPatternDao]).to(classOf[ReportCardEvaluationPatternDaoImpl]).in(classOf[Singleton])
    bind(classOf[ReportCardRescheduledDao]).to(classOf[ReportCardRescheduledDaoImpl]).in(classOf[Singleton])
    bind(classOf[ReportCardRetryDao]).to(classOf[ReportCardRetryDaoImpl]).in(classOf[Singleton])
    bind(classOf[RoleDao]).to(classOf[RoleDaoImpl]).in(classOf[Singleton])
    bind(classOf[ScheduleEntryDao]).to(classOf[ScheduleEntryDaoImpl]).in(classOf[Singleton])
    bind(classOf[TimetableDao]).to(classOf[TimetableDaoImpl]).in(classOf[Singleton])
    bind(classOf[UserDao]).to(classOf[UserDaoImpl]).in(classOf[Singleton])
    bind(classOf[DashboardDao]).to(classOf[DashboardDaoImpl]).in(classOf[Singleton])
  }

  private def bindActors(): Unit = {
    bind(classOf[ActorScheduler]).asEagerSingleton()

    bind(classOf[ActorRef])
      .annotatedWith(classOf[BackupServiceActorAnnotation])
      .toProvider(classOf[BackupServiceProvider])
    bind(classOf[Any])
      .annotatedWith(Names.named("backupMessage"))
      .toInstance(BackupServiceActor.BackupRequestAsync)
    bindConstant()
      .annotatedWith(Names.named("backupFireTime"))
      .to(config("lwm.backup.localTime") getOrElse "")
  }

  private def bindDatabase(): Unit = {
    bind(classOf[Database]).toProvider(classOf[DatabaseProvider])
    bind(classOf[DatabaseCloseHook]).asEagerSingleton()
  }

  private def config(name: String): Option[String] = config getOptional[String] name
}