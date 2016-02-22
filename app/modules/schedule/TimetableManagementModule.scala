package modules.schedule

import controllers.crud.schedule.TimetableCRUDController
import modules.security.SecurityManagementModule
import modules.semester.BlacklistServiceManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.{TimetableService, TimetableServiceLike}
import utils.LwmApplication

trait TimetableServiceManagementModule {
  self: LwmApplication with BlacklistServiceManagementModule =>

  def timetableService: TimetableServiceLike
}

trait DefaultTimetableServiceManagementModule extends TimetableServiceManagementModule {
  self: LwmApplication with BlacklistServiceManagementModule =>

  lazy val timetableService: TimetableServiceLike = new TimetableService(blacklistService)
}

trait TimetableManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def timetableManagementController: TimetableCRUDController
}

trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, namespace, roleService)
}