package modules.labwork.schedule

import controllers.schedule.ScheduleController
import modules.SessionRepositoryModule
import modules.labwork.GroupServiceManagementModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services._
import utils.LwmApplication

trait ScheduleServiceManagementModule {
  self: LwmApplication =>

  def scheduleService: ScheduleGenesisServiceLike
}

trait DefaultScheduleServiceManagementModule extends ScheduleServiceManagementModule {
  self: LwmApplication with TimetableServiceManagementModule =>

  lazy val scheduleService: ScheduleGenesisServiceLike = new ScheduleService(timetableService)
}

trait ScheduleManagementModule {

  def scheduleManagementController: ScheduleController
}

trait DefaultScheduleManagementModuleImpl extends ScheduleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ScheduleServiceManagementModule with GroupServiceManagementModule =>

  lazy val scheduleManagementController: ScheduleController = new ScheduleController(repository, sessionService, namespace, roleService, scheduleService, groupService)
}