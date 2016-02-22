package modules

import controllers.crud.LabworkApplicationCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.{LabworkApplicationService, LabworkApplicationServiceLike}
import utils.LwmApplication

trait LabworkApplicationServiceModule {
  self: LwmApplication with SemanticRepositoryModule =>
  def labworkApplicationService: LabworkApplicationServiceLike
}

trait DefaultLabworkApplicationServiceModule extends LabworkApplicationServiceModule {
  self: LwmApplication with SemanticRepositoryModule =>
  override lazy val labworkApplicationService: LabworkApplicationServiceLike = LabworkApplicationService(repository)
}

trait LabworkApplicationManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>
  def labworkApplicationController: LabworkApplicationCRUDController
}

trait DefaultLabworkApplicationManagementModule extends LabworkApplicationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  override lazy val labworkApplicationController: LabworkApplicationCRUDController = new LabworkApplicationCRUDController(repository, namespace, roleService)
}
