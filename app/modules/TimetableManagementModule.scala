package modules

import controllers.TimetableCRUDController
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
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def timetableManagementController: TimetableCRUDController
}

trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, sessionService, namespace, roleService)
}