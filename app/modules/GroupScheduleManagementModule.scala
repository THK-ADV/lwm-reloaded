//package modules
//
//import controllers.crud.GroupScheduleCRUDController
//
//
//trait GroupScheduleManagementModule {
//  self: SemanticRepositoryModule =>
//  def groupScheduleManagementController: GroupScheduleCRUDController
//}
//
//trait DefaultGroupScheduleManagementModuleImpl extends GroupScheduleManagementModule {
//  self: SemanticRepositoryModule =>
//  lazy val groupScheduleManagementController: GroupScheduleCRUDController = new GroupScheduleCRUDController(repository, namespace)
//}