package modules.schedule

import controllers.crud.schedule.ScheduleCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services._
import utils.LwmApplication

trait ScheduleServiceManagementModule {
  self: LwmApplication =>

  def scheduleService: ScheduleService
}

trait DefaultScheduleServiceManagementModule extends ScheduleServiceManagementModule {
  self: LwmApplication =>

  lazy val scheduleService: ScheduleService = new ScheduleService
}

trait ScheduleGenesisServiceManagementModule {
  self: LwmApplication =>

  def scheduleGenesisService: ScheduleGenesisServiceLike
}

trait DefaultScheduleGenesisServiceManagementModule extends ScheduleGenesisServiceManagementModule {
  self: LwmApplication with ScheduleServiceManagementModule =>

  lazy val scheduleGenesisService: ScheduleGenesisServiceLike = new ScheduleGenesisService(scheduleService)
}

trait ScheduleManagementModule {
  self: SemanticRepositoryModule =>

  def scheduleManagementController: ScheduleCRUDController
}

trait DefaultScheduleManagementModuleImpl extends ScheduleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with ScheduleGenesisServiceManagementModule =>

  lazy val scheduleManagementController: ScheduleCRUDController = new ScheduleCRUDController(repository, namespace, roleService, scheduleGenesisService)
}