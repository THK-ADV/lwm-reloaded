package modules.schedule

import controllers.crud.schedule.TimetableCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait TimetableManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def timetableManagementController: TimetableCRUDController
}

trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, namespace, roleService)
}