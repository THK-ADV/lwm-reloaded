package modules

import controllers.crud.TimetableEntryCRUDController


//trait TimetableManagementModule {
//  self: SemanticRepositoryModule =>
//  def timetableManagementController: TimetableCRUDController
//}
//
//trait DefaultTimetableManagementModuleImpl extends TimetableManagementModule {
//  self: SemanticRepositoryModule =>
//  lazy val timetableManagementController: TimetableCRUDController = new TimetableCRUDController(repository, namespace)
//}

trait TimetableEntryManagementModule {
  self: SemanticRepositoryModule =>
  def timetableEntryManagementController: TimetableEntryCRUDController
}

trait DefaultTimetableEntryManagementModuleImpl extends TimetableEntryManagementModule {
  self: SemanticRepositoryModule with BaseNamespace =>
  lazy val timetableEntryManagementController: TimetableEntryCRUDController = new TimetableEntryCRUDController(repository, namespace)
}
