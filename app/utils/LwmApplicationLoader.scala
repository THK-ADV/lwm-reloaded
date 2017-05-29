package utils

import controllers._
import modules._
import play.api.ApplicationLoader.Context
import play.api.mvc.EssentialFilter
import play.api.routing.Router
import play.api.{Application, ApplicationLoader, BuiltInComponentsFromContext}
import router.Routes

class LwmApplicationLoader extends ApplicationLoader {
  def load(context: Context): Application = {
    new DefaultLwmApplication(context).application
  }
}

trait DefaultHomepageModuleImpl extends HomepageModule {
  lazy val homepageController = new HomepageController
}

trait HomepageModule {
  def homepageController: HomepageController
}

trait AssetsModule {
  self: LwmApplication =>
  def assetsController: Assets
}

trait DefaultAssetsModuleImpl extends AssetsModule {
  self: LwmApplication =>
  lazy val assetsController = new Assets(httpErrorHandler)
}

abstract class LwmApplication(context: Context) extends BuiltInComponentsFromContext(context)
with ConfigurationModule
with BaseNamespace
with HomepageModule
with SemanticRepositoryModule
with DatabaseModule
with DbFolder
with DegreeManagementModule
with DegreeManagementModulePostgres
with DegreeServiceModule
with CourseManagementModule
with CourseManagementModulePostgres
with CourseServiceModule
with GroupServiceManagementModule
with GroupManagementModule
with LabworkManagementModule
with LabworkManagementModulePostgres
with LabworkServiceModule
with RoomManagementModule
with RoomManagementModulePostgres
with RoomServiceModule
with SemesterManagementModule
with SemesterManagementModulePostgres
with SemesterServiceModule
with SessionRepositoryModule
with SecurityManagementModule
with RoleManagementModule
with RoleServiceModule
with AuthorityManagementModule
with AuthorityServiceModule
with PermissionManagementModule
with PermissionServiceModule
with SessionControllerModule
with AkkaActorSystemModule
with LdapModule
with AssetsModule
with EntryTypeManagementModule
with ResolversModule
with CORSFilterModule
with ApiDataModule
with LabworkApplicationManagementModule
with LabworkApplicationManagementModulePostgres
with LabworkApplicationServiceModule
with LabworkApplication2ServiceModule
with ScheduleManagementModule
with ScheduleEntryManagementModule
with TimetableManagementModule
with TimetableServiceManagementModule
with ScheduleServiceManagementModule
with BlacklistManagementModule
with BlacklistServiceManagementModule
with ReportCardServiceManagementModule
with ReportCardEntryManagementModule
with ReportCardEntryTypeManagementModule
with AssignmentPlanManagementModule
with AssignmentPlanManagementModulePostgres
with AssignmentPlanServiceModule
with UserManagementModule
with UserManagementModulePostgres
with UserServiceModule
with AnnotationManagementModule
with ReportCardEvaluationManagementModule
with LdapSyncModule {
  override lazy val httpFilters: Seq[EssentialFilter] = Seq(corsFilter(context.initialConfiguration))

  lazy val router: Router = new Routes(
    httpErrorHandler,
    homepageController,
    degreeManagementController,
    degreeManagementControllerPostgres,
    courseManagementController,
    courseManagementControllerPostgres,
    groupManagementController,
    labworkManagementController,
    labworControllerPostgres,
    entryTypeController,
    roomManagementController,
    roomManagementControllerPostgres,
    semesterManagementController,
    semesterManagementControllerPostgres,
    roleManagementController,
    authorityManagementController,
    permissionManagementController,
    labworkApplicationController,
    labworkApplicationControllerPostgres,
    scheduleManagementController,
    scheduleEntryController,
    timetableManagementController,
    blacklistManagementController,
    reportCardEntryManagementController,
    reportCardEntryTypeManagementController,
    reportCardEvaluationManagementController,
    assignmentPlanManagementController,
    assignmentPlanManagementControllerPostgres,
    annotationManagementController,
    userController,
    userControllerPostgres,
    sessionController,
    apiDataController,
    assetsController
  )
}

class DefaultLwmApplication(context: Context) extends LwmApplication(context)
with ConfigurationModuleImpl
with ConfigurableBaseNamespace
with DefaultSemanticRepositoryModuleImpl
with DefaultDatabaseModule
with DefaultHomepageModuleImpl
with DefaultDegreeManagementModuleImpl
with DefaultDegreeManagementModuleImplPostgres
with DefaultDegreeServiceModule
with DefaultCourseManagementModuleImpl
with DefaultCourseManagementModuleImplPostgres
with DefaultCourseServiceModule
with DefaultGroupServiceManagementModule
with DefaultGroupManagementModuleImpl
with DefaultLabworkManagementModuleImpl
with DefaultLabworkManagementModulePostgres
with DefaultLabworkServiceModule
with DefaultRoomManagementModuleImpl
with DefaultRoomManagementModuleImplPostgres
with DefaultRoomServiceModule
with DefaultSemesterManagementModuleImpl
with DefaultSemesterManagementModuleImplPostgres
with DefaultSemesterServiceModule
with LdapModuleImpl
with DefaultSessionRepositoryModuleImpl
with DefaultAssetsModuleImpl
with DefaultRoleManagementModule
with DefaultRoleServiceModule
with DefaultAuthorityManagementModuleImpl
with DefaultAuthorityServiceModule
with DefaultPermissionManagementModule
with DefaultPermissionServiceModule
with DefaultSessionControllerModuleImpl
with DefaultSecurityManagementModule
with DefaultEntryTypeManagementModule
with DefaultResolversModule
with DefaultCORSFilterModule
with DefaultApiDataModule
with DefaultLabworkApplicationManagementModule
with DefaultLabworkApplicationManagementModulePostgres
with DefaultLabworkApplicationServiceModule
with DefaultLabworkApplication2ServiceModule
with DefaultScheduleManagementModuleImpl
with DefaultScheduleEntryManagementModule
with DefaultTimetableManagementModuleImpl
with DefaultTimetableServiceManagementModule
with DefaultScheduleServiceManagementModule
with DefaultBlacklistManagementModuleImpl
with DefaultBlacklistServiceManagementModule
with DefaultReportCardServiceManagementModule
with DefaultReportCardEntryManagementModuleImpl
with DefaultReportCardEntryTypeManagementModuleImpl
with DefaultAssignmentPlanManagementModuleImpl
with DefaultAssignmentPlanManagementModuleImplPostgres
with DefaultAssignmentPlanServiceModule
with DefaultUserManagementModule
with DefaultUserManagementModulePostgres
with DefaultUserServiceModule
with DefaultAnnotationManagementModuleImpl
with DefaultReportCardEvaluationManagementModuleImpl
with DefaultDbFolderImpl
with DefaultDbBackupModuleImpl
with DefaultLdapSyncService