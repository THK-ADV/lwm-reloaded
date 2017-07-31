package modules

import controllers.{RoomCRUDController, RoomControllerPostgres}
import dao.{RoomService, RoomServiceImpl}

trait RoomManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait RoomServiceModule { self: DatabaseModule =>
  def roomService: RoomService
}

trait DefaultRoomServiceModule extends RoomServiceModule { self: DatabaseModule =>
  override lazy val roomService = new RoomServiceImpl(db)
}

trait RoomManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with RoomServiceModule =>

  def roomManagementControllerPostgres: RoomControllerPostgres
}

trait DefaultRoomManagementModuleImplPostgres extends RoomManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with RoomServiceModule =>

  lazy val roomManagementControllerPostgres: RoomControllerPostgres = new RoomControllerPostgres(sessionService, roleService, roomService)
}