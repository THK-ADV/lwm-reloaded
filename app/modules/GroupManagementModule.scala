package modules

import controllers.GroupCRUDController
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