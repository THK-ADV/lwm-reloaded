package modules

import controllers.RoomCRUDController

trait RoomManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, sessionService, namespace, roleService)
}