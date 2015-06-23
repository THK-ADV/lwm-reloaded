package utils

import akka.actor.ActorSystem
import controllers._
import controllers.crud._
import play.api.ApplicationLoader.Context
import play.api.routing.Router
import play.api.{Application, ApplicationLoader, BuiltInComponentsFromContext}
import router.Routes
import store.{Namespace, SesameRepository}

class LwmApplicationLoader extends ApplicationLoader {
  def load(context: Context): Application = {
    new DefaultLwmApplication(context).application
  }
}

trait AkkaActorSystemModule {
  implicit def system = ActorSystem("lwm-system")
}

trait SessionControllerModule {
  self: SessionRepositoryModule =>
  def sessionController: SessionManagement = new SessionManagement(sessionRepository)
}

trait SessionRepositoryModule {
  def sessionRepository: SessionHandling
}

trait DefaultSessionRepositoryModuleImpl extends SessionRepositoryModule {
  self: AkkaActorSystemModule =>

  override def sessionRepository: SessionHandling = new SessionRepository(system)
}

trait SemanticRepositoryModule {
  def repository: SesameRepository

  def namespace: Namespace
}

trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  def namespace: Namespace = Namespace("http://lwm/")

  def repository: SesameRepository = SesameRepository(namespace)
}

trait DegreeManagementModule {
  self: SemanticRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule =>
  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, namespace)
}

trait CourseManagementModule {
  self: SemanticRepositoryModule =>
  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule =>
  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, namespace)
}

trait EmployeeManagementModule {
  self: SemanticRepositoryModule =>
  def employeeManagementController: EmployeeCRUDController
}

trait DefaultEmployeeManagementModuleImpl extends EmployeeManagementModule {
  self: SemanticRepositoryModule =>
  lazy val employeeManagementController: EmployeeCRUDController = new EmployeeCRUDController(repository, namespace)
}

trait GroupManagementModule {
  self: SemanticRepositoryModule =>
  def groupManagementController: GroupCRUDController
}

trait DefaultGroupManagementModuleImpl extends GroupManagementModule {
  self: SemanticRepositoryModule =>
  lazy val groupManagementController: GroupCRUDController = new GroupCRUDController(repository, namespace)
}

trait GroupScheduleAssociationManagementModule {
  self: SemanticRepositoryModule =>
  def groupScheduleAssociationManagementController: GroupScheduleAssociationCRUDController
}

trait DefaultGroupScheduleAssociationManagementModuleImpl extends GroupScheduleAssociationManagementModule {
  self: SemanticRepositoryModule =>
  lazy val groupScheduleAssociationManagementController: GroupScheduleAssociationCRUDController = new GroupScheduleAssociationCRUDController(repository, namespace)
}

trait GroupScheduleManagementModule {
  self: SemanticRepositoryModule =>
  def groupScheduleManagementController: GroupScheduleCRUDController
}

trait DefaultGroupScheduleManagementModuleImpl extends GroupScheduleManagementModule {
  self: SemanticRepositoryModule =>
  lazy val groupScheduleManagementController: GroupScheduleCRUDController = new GroupScheduleCRUDController(repository, namespace)
}

trait LabworkManagementModule {
  self: SemanticRepositoryModule =>
  def labworkManagementController: LabworkCRUDController
}

trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule =>
  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, namespace)
}

trait RoomManagementModule {
  self: SemanticRepositoryModule =>
  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule =>
  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, namespace)
}

trait SemesterManagementModule {
  self: SemanticRepositoryModule =>
  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule =>
  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, namespace)
}

trait StudentManagementModule {
  self: SemanticRepositoryModule =>
  def studentManagementController: StudentCRUDController
}

trait DefaultStudentManagementModuleImpl extends StudentManagementModule {
  self: SemanticRepositoryModule =>
  lazy val studentManagementController: StudentCRUDController = new StudentCRUDController(repository, namespace)
}

trait StudentScheduleAssociationManagementModule {
  self: SemanticRepositoryModule =>
  def studentScheduleAssociationManagementController: StudentScheduleAssociationCRUDController
}

trait DefaultStudentScheduleAssociationManagementModuleImpl extends StudentScheduleAssociationManagementModule {
  self: SemanticRepositoryModule =>
  lazy val studentScheduleAssociationManagementController: StudentScheduleAssociationCRUDController = new StudentScheduleAssociationCRUDController(repository, namespace)
}

trait StudentScheduleManagementModule {
  self: SemanticRepositoryModule =>
  def studentScheduleManagementController: StudentScheduleCRUDController
}

trait DefaultStudentScheduleManagementModuleImpl extends StudentScheduleManagementModule {
  self: SemanticRepositoryModule =>
  lazy val studentScheduleManagementController: StudentScheduleCRUDController = new StudentScheduleCRUDController(repository, namespace)
}

trait TimetableManagementModule {
  self: SemanticRepositoryModule =>
  def timetableManagementController: TimetableCRUDController
}

trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
  self: SemanticRepositoryModule =>
  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, namespace)
}

trait TimetableEntryManagementModule {
  self: SemanticRepositoryModule =>
  def timetableEntryManagementController: TimetableEntryCRUDController
}

trait DefaultTimetableEntryManagementModuleImpl extends TimetableEntryManagementModule {
  self: SemanticRepositoryModule =>
  lazy val timetableEntryManagementController: TimetableEntryCRUDController = new TimetableEntryCRUDController(repository, namespace)
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
with HomepageModule
with SemanticRepositoryModule
with DegreeManagementModule
with CourseManagementModule
with EmployeeManagementModule
with GroupManagementModule
with GroupScheduleAssociationManagementModule
with GroupScheduleManagementModule
with LabworkManagementModule
with RoomManagementModule
with SemesterManagementModule
with StudentManagementModule
with StudentScheduleAssociationManagementModule
with StudentScheduleManagementModule
with TimetableManagementModule
with TimetableEntryManagementModule
with SessionRepositoryModule
with SessionControllerModule
with AkkaActorSystemModule
with AssetsModule {
  lazy val router: Router = new Routes(
    httpErrorHandler,
    homepageController,
    degreeManagementController,
    courseManagementController,
    employeeManagementController,
    groupManagementController,
    groupScheduleAssociationManagementController,
    groupScheduleManagementController,
    labworkManagementController,
    roomManagementController,
    semesterManagementController,
    studentManagementController,
    studentScheduleAssociationManagementController,
    studentScheduleManagementController,
    timetableManagementController,
    timetableEntryManagementController,
    sessionController,
    assetsController
  )
}

class DefaultLwmApplication(context: Context) extends LwmApplication(context)
with DefaultSemanticRepositoryModuleImpl
with DefaultHomepageModuleImpl
with DefaultDegreeManagementModuleImpl
with DefaultCourseManagementModuleImpl
with DefaultEmployeeManagementModuleImpl
with DefaultGroupManagementModuleImpl
with DefaultGroupScheduleAssociationManagementModuleImpl
with DefaultGroupScheduleManagementModuleImpl
with DefaultLabworkManagementModuleImpl
with DefaultRoomManagementModuleImpl
with DefaultSemesterManagementModuleImpl
with DefaultStudentManagementModuleImpl
with DefaultStudentScheduleAssociationManagementModuleImpl
with DefaultStudentScheduleManagementModuleImpl
with DefaultTimetableManagementModuleImpl
with DefaultTimetableEntryManagementModuleImpl
with DefaultSessionRepositoryModuleImpl
with DefaultAssetsModuleImpl