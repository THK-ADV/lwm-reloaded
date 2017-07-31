package modules

import controllers.{GroupCRUDController, GroupControllerPostgres}
import dao.{GroupDao, GroupDaoImpl}
import services.{GroupService, GroupServiceLike}
import utils.LwmApplication

trait GroupServiceManagementModule {
  self: LwmApplication with LabworkApplicationServiceModule =>

  def groupService: GroupServiceLike
}

trait DefaultGroupServiceManagementModule extends GroupServiceManagementModule {
  self: LwmApplication with LabworkApplicationServiceModule =>

  lazy val groupService: GroupServiceLike = new GroupService(labworkApplicationService)
}

trait GroupManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with GroupServiceManagementModule with SessionRepositoryModule =>

  def groupManagementController: GroupCRUDController
}

trait DefaultGroupManagementModuleImpl extends GroupManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with GroupServiceManagementModule with SessionRepositoryModule =>

  lazy val groupManagementController: GroupCRUDController = new GroupCRUDController(repository, sessionService, namespace, roleService, groupService)
}

// POSTGRES

trait GroupDaoManagementModule {
  self: LwmApplication with DatabaseModule =>

  def groupDao: GroupDao
}

trait DefaultGroupDaoManagementModule extends GroupDaoManagementModule {
  self: LwmApplication with DatabaseModule =>

  override lazy val groupDao = new GroupDaoImpl(db)
}

trait GroupManagementModule2 {
  self: SecurityManagementModule with SessionRepositoryModule with GroupDaoManagementModule with LabworkApplication2ServiceModule =>

  def groupManagementControllerPostgres: GroupControllerPostgres
}

trait DefaultGroupManagementModule2 extends GroupManagementModule2 {
  self: SecurityManagementModule with SessionRepositoryModule with GroupDaoManagementModule with LabworkApplication2ServiceModule =>

  override lazy val groupManagementControllerPostgres = new GroupControllerPostgres(roleService, sessionService, groupDao, labworkApplicationService2)
}