import java.io.File

import akka.actor.{ActorRef, ActorSystem, Props}
import auth.{KeycloakAuthorization, OAuthAuthorization}
import com.google.inject.name.Names
import com.google.inject.{AbstractModule, Provides}
import com.typesafe.config.ConfigFactory
import dao._
import javax.inject.{Named, Singleton}
import org.keycloak.adapters.{KeycloakDeployment, KeycloakDeploymentBuilder}
import play.api.{Configuration, Play}
import services.backup.{BackupService, BackupServiceActor, PSQLBackupService}
import services.blacklist.{BlacklistService, BlacklistServiceImpl}
import services.{ActorScheduler, ScheduleService, ScheduleServiceImpl, Webservice}
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.util.Try

class Module extends AbstractModule {

  import Module._

  override def configure(): Unit = {
    bindDaos()
    bindServices()
    bindActors()
  }

  private def bindServices(): Unit = {
    bind(classOf[Webservice]).in(classOf[Singleton])
    bind(classOf[BlacklistService]).to(classOf[BlacklistServiceImpl]).in(classOf[Singleton])
    bind(classOf[OAuthAuthorization]).to(classOf[KeycloakAuthorization]).in(classOf[Singleton])
    bind(classOf[BackupService]).to(classOf[PSQLBackupService]).in(classOf[Singleton])
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

    bind(classOf[Any])
      .annotatedWith(Names.named("backupMessage"))
      .toInstance(BackupServiceActor.BackupRequestAsync)
    bindConstant()
      .annotatedWith(Names.named("backupFireTime"))
      .to(config("lwm.backup.localTime") getOrElse "")
  }

  @Provides
  @Singleton
  private def databaseProvider(): PostgresProfile.backend.Database = Database forConfig "database"

  @Provides
  @Singleton
  private def keycloakDeploymentProvider(): KeycloakDeployment = KeycloakDeploymentBuilder.build(
    Play.getClass.getResourceAsStream("/keycloak.json")
  )

  @Provides
  @Singleton
  private def scheduleServiceProvider(): ScheduleService = {
    val pops = _config getOptional[Int] "lwm.schedule.populations" getOrElse 20
    val gens = _config getOptional[Int] "lwm.schedule.generations" getOrElse 100
    val elites = _config getOptional[Int] "lwm.schedule.elites" getOrElse 10

    new ScheduleServiceImpl(pops, gens, elites)
  }

  @Provides
  @Singleton
  @Named("backupActor")
  private def backupServiceActor(system: ActorSystem, backupService: BackupService): ActorRef = {
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

object Module {
  private lazy val _config = Configuration(ConfigFactory.defaultApplication.resolve)

  private def nonEmptyConfig(name: String): Option[String] = config(name) filter (_.nonEmpty)

  private def config(name: String): Option[String] = _config getOptional[String] name
}