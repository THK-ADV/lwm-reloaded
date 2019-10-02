import akka.actor.ActorRef
import auth.{KeycloakAuthorization, OAuthAuthorization}
import com.google.inject.AbstractModule
import com.google.inject.name.Names
import dao._
import di._
import javax.inject.Singleton
import org.keycloak.adapters.KeycloakDeployment
import play.api.{Configuration, Environment}
import security.{SecurityActionChain, SecurityActionChainImpl}
import service.actor.{ActorScheduler, BackupServiceActor, BlacklistApiServiceActor, SemesterCreationActor}
import service.backup.{BackupService, PSQLBackupService}
import service._
import slick.jdbc.PostgresProfile.api._

class Module(environment: Environment, implicit val config: Configuration) extends AbstractModule with ConfigReader {

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

    bind(classOf[SemesterService]).in(classOf[Singleton])

    bind(classOf[SecurityActionChain]).to(classOf[SecurityActionChainImpl]).in(classOf[Singleton])
  }

  private def bindDaos(): Unit = {
    bind(classOf[SemesterDao]).to(classOf[SemesterDaoImpl]).in(classOf[Singleton])
    bind(classOf[AuthorityDao]).to(classOf[AuthorityDaoImpl]).in(classOf[Singleton])
    bind(classOf[RoleDao]).to(classOf[RoleDaoImpl]).in(classOf[Singleton])
    bind(classOf[AssignmentEntryDao]).to(classOf[AssignmentEntryDaoImpl]).in(classOf[Singleton])
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
    bind(classOf[LwmServiceDao]).to(classOf[LwmServiceDaoImpl]).in(classOf[Singleton])
  }

  private def bindActors(): Unit = {
    bind(classOf[ActorScheduler]).asEagerSingleton()

    bind(classOf[ActorRef])
      .annotatedWith(classOf[BackupServiceActorAnnotation])
      .toProvider(classOf[BackupServiceActorProvider])
    bind(classOf[Any])
      .annotatedWith(Names.named("backupMessage"))
      .toInstance(BackupServiceActor.BackupRequestAsync)
    bindConstant()
      .annotatedWith(Names.named("backupFireTime"))
      .to(config("lwm.backup.localTime") getOrElse "")

    bind(classOf[ActorRef])
      .annotatedWith(classOf[SemesterCreationActorAnnotation])
      .toProvider(classOf[SemesterCreationActorProvider])
    bind(classOf[Any])
      .annotatedWith(Names.named("semesterCreationMessage"))
      .toInstance(SemesterCreationActor.CreationRequestAsync)
    bindConstant()
      .annotatedWith(Names.named("semesterCreationFireTime"))
      .to(config("lwm.semester.localTime") getOrElse "")

    bind(classOf[ActorRef])
      .annotatedWith(classOf[BlacklistApiServiceActorAnnotation])
      .toProvider(classOf[BlacklistApiServiceActorProvider])
    bind(classOf[Any])
      .annotatedWith(Names.named("blacklistDownloadMessage"))
      .toInstance(BlacklistApiServiceActor.BlacklistDownloadRequestSync)
    bindConstant()
      .annotatedWith(Names.named("blacklistDownloadFireTime"))
      .to(config("lwm.blacklist.localTime") getOrElse "")
  }

  private def bindDatabase(): Unit = {
    bind(classOf[Database]).toProvider(classOf[DatabaseProvider])
    bind(classOf[DatabaseCloseHook]).asEagerSingleton()
  }
}