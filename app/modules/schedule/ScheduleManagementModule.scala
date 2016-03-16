package modules.schedule

import controllers.crud.schedule.ScheduleCRUDController
import modules.ReportCardServiceManagementModule
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
  self: SemanticRepositoryModule =>

  def scheduleManagementController: ScheduleCRUDController
}

trait DefaultScheduleManagementModuleImpl extends ScheduleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with ScheduleServiceManagementModule with ReportCardServiceManagementModule =>

  lazy val scheduleManagementController: ScheduleCRUDController = new ScheduleCRUDController(repository, namespace, roleService, scheduleService, reportCardService)
}