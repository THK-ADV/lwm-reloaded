package modules

import controllers.crud.GroupCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.{GroupServiceLike, GroupService}
import utils.LwmApplication

trait GroupServiceManagementModule {
  self: LwmApplication with SemanticRepositoryModule with LabworkApplicationServiceModule =>

  def groupService: GroupServiceLike
}

trait DefaultGroupServiceManagementModule extends GroupServiceManagementModule {
  self: LwmApplication with SemanticRepositoryModule with LabworkApplicationServiceModule =>

  lazy val groupService: GroupServiceLike = new GroupService(repository, labworkApplicationService)
}

trait GroupManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with GroupServiceManagementModule =>

  def groupManagementController: GroupCRUDController
}

trait DefaultGroupManagementModuleImpl extends GroupManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with GroupServiceManagementModule =>

  lazy val groupManagementController: GroupCRUDController = new GroupCRUDController(repository, namespace, roleService, groupService)
}
