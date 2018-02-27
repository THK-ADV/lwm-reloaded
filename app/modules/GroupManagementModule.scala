package modules

import controllers.GroupControllerPostgres
import dao.{GroupDao, GroupDaoImpl}

trait GroupDaoManagementModule {
  self: DatabaseModule =>

  def groupDao: GroupDao
}

trait DefaultGroupDaoManagementModule extends GroupDaoManagementModule {
  self: DatabaseModule =>

  override lazy val groupDao = new GroupDaoImpl(db)
}

trait GroupManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with GroupDaoManagementModule with LabworkApplicationDaoModule =>

  def groupManagementControllerPostgres: GroupControllerPostgres
}

trait DefaultGroupManagementModule2 extends GroupManagementModule2 {
  self: AuthorityDaoModule with SessionRepositoryModule with GroupDaoManagementModule with LabworkApplicationDaoModule =>

  override lazy val groupManagementControllerPostgres = new GroupControllerPostgres(authorityDao, sessionService, groupDao, labworkApplicationDao)
}