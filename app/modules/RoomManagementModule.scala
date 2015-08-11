package modules

import controllers.crud.RoomCRUDController


trait RoomManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>
  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, namespace, roleService)
}
