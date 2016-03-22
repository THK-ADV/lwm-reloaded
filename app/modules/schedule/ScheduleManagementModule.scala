package modules.schedule

import controllers.crud.schedule.ScheduleCRUDController
import modules.SessionRepositoryModule
import modules.reportCard.ReportCardServiceManagementModule
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
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def scheduleManagementController: ScheduleCRUDController
}

trait DefaultScheduleManagementModuleImpl extends ScheduleManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule with ScheduleServiceManagementModule with ReportCardServiceManagementModule =>

  lazy val scheduleManagementController: ScheduleCRUDController = new ScheduleCRUDController(repository, sessionService, namespace, roleService, scheduleService, reportCardService)
}