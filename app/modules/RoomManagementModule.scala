package modules

import controllers.RoomControllerPostgres
import dao.{RoomDao, RoomDaoImpl}

trait RoomDaoModule {
  self: DatabaseModule =>

  def roomDao: RoomDao
}

trait DefaultRoomDaoModule extends RoomDaoModule {
  self: DatabaseModule =>

  override lazy val roomDao = new RoomDaoImpl(db)
}

trait RoomManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with RoomDaoModule =>

  def roomManagementControllerPostgres: RoomControllerPostgres
}

trait DefaultRoomManagementModuleImplPostgres extends RoomManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with RoomDaoModule =>

  lazy val roomManagementControllerPostgres: RoomControllerPostgres = new RoomControllerPostgres(sessionService, authorityDao, roomDao)
}