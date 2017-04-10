package modules

import controllers.{RoomCRUDController, RoomControllerPostgres}
import services.RoomService

trait RoomManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, sessionService, namespace, roleService)
}

trait RoomManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  def roomManagementControllerPostgres: RoomControllerPostgres
}

trait DefaultRoomManagementModuleImplPostgres extends RoomManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  lazy val roomManagementControllerPostgres: RoomControllerPostgres = new RoomControllerPostgres(sessionService, roleService, RoomService)
}