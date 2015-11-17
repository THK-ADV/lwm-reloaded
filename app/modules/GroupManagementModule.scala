package modules

import controllers.crud.GroupCRUDController
import services.{GroupServiceLike, GroupService}
import utils.LwmApplication

trait GroupServiceManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>

  def groupService: GroupServiceLike
}

trait DefaultGroupServiceManagementModule extends GroupServiceManagementModule {
  self: LwmApplication with SemanticRepositoryModule =>

  lazy val groupService: GroupServiceLike = new GroupService(repository)
}

trait GroupManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with GroupServiceManagementModule =>

  def groupManagementController: GroupCRUDController
}

trait DefaultGroupManagementModuleImpl extends GroupManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with GroupServiceManagementModule =>

  lazy val groupManagementController: GroupCRUDController = new GroupCRUDController(repository, namespace, roleService, groupService)
}
