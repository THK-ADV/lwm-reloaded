package modules

import controllers.GroupControllerPostgres
import dao.{GroupDao, GroupDaoImpl}
import utils.LwmApplication

trait GroupDaoManagementModule {
  self: LwmApplication with DatabaseModule =>

  def groupDao: GroupDao
}

trait DefaultGroupDaoManagementModule extends GroupDaoManagementModule {
  self: LwmApplication with DatabaseModule =>

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