package modules

import controllers.crud.RoomCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}


trait RoomManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, namespace, roleService)
}
