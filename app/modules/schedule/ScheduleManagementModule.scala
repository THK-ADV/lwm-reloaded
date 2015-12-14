package modules.schedule

import controllers.crud.schedule.ScheduleCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.{TimetableService, TimetableServiceLike}
import utils.LwmApplication

trait TimetableServiceManagementModule {
  self: LwmApplication =>

  def timetableService: TimetableServiceLike
}

trait DefaultTimetableServiceManagementModule extends TimetableServiceManagementModule {
  self: LwmApplication =>

  lazy val timetableService: TimetableServiceLike = new TimetableService()
}

trait ScheduleManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with TimetableServiceManagementModule =>

  def scheduleManagementController: ScheduleCRUDController
}

trait DefaultScheduleManagementModuleImpl extends ScheduleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with TimetableServiceManagementModule =>

  lazy val scheduleManagementController: ScheduleCRUDController = new ScheduleCRUDController(repository, namespace, roleService, timetableService)
}