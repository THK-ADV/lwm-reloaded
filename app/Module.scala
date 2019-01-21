import auth.{KeycloakAuthorization, OAuthAuthorization}
import com.google.inject.{AbstractModule, Provides}
import dao._
import javax.inject.Singleton
import org.keycloak.adapters.{KeycloakDeployment, KeycloakDeploymentBuilder}
import play.api.{Configuration, Play}
import services.blacklist.{BlacklistService, BlacklistServiceImpl}
import services.{ScheduleService, ScheduleServiceImpl, Webservice}
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

class Module extends AbstractModule {

  override def configure(): Unit = {
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

    bind(classOf[BlacklistService]).to(classOf[BlacklistServiceImpl]).in(classOf[Singleton])
    bind(classOf[OAuthAuthorization]).to(classOf[KeycloakAuthorization]).in(classOf[Singleton])
    bind(classOf[Webservice]).in(classOf[Singleton])
  }

  private lazy val config = Configuration.reference

  @Provides
  @Singleton
  private def databaseProvider(): PostgresProfile.backend.Database = Database.forConfig("database")

  @Provides
  @Singleton
  private def keycloakDeploymentProvider(): KeycloakDeployment = KeycloakDeploymentBuilder.build(
    Play.getClass.getResourceAsStream("/keycloak.json")
  )

  @Provides
  @Singleton
  private def scheduleServiceProvider(): ScheduleService = {
    val pops = config getOptional[Int] "lwm.schedule.populations" getOrElse 20
    val gens = config getOptional[Int] "lwm.schedule.generations" getOrElse 100
    val elites = config getOptional[Int] "lwm.schedule.elites" getOrElse 10

    new ScheduleServiceImpl(pops, gens, elites)
  }
}