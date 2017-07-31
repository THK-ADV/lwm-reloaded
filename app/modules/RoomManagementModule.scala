package modules

import controllers.{RoomCRUDController, RoomControllerPostgres}
import dao.{RoomDao, RoomDaoImpl}

trait RoomManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def roomManagementController: RoomCRUDController
}

trait DefaultRoomManagementModuleImpl extends RoomManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val roomManagementController: RoomCRUDController = new RoomCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait RoomDaoModule { self: DatabaseModule =>
  def roomDao: RoomDao
}

trait DefaultRoomDaoModule extends RoomDaoModule { self: DatabaseModule =>
  override lazy val roomDao = new RoomDaoImpl(db)
}

trait RoomManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with RoomDaoModule =>

  def roomManagementControllerPostgres: RoomControllerPostgres
}

trait DefaultRoomManagementModuleImplPostgres extends RoomManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with RoomDaoModule =>

  lazy val roomManagementControllerPostgres: RoomControllerPostgres = new RoomControllerPostgres(sessionService, roleService, roomDao)
}