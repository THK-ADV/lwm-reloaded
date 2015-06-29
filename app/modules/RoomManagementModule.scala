package modules

import controllers.crud.RoomCRUDController


trait RoomManagementModule {
  self: SemanticRepositoryModule =>
  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule =>
  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, namespace)
}