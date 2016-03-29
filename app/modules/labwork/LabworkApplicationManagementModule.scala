package modules.labwork

import controllers.crud.labwork.LabworkApplicationCRUDController
import modules.SessionRepositoryModule
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
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def labworkApplicationController: LabworkApplicationCRUDController
}

trait DefaultLabworkApplicationManagementModule extends LabworkApplicationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>
  override lazy val labworkApplicationController: LabworkApplicationCRUDController = new LabworkApplicationCRUDController(repository, sessionService, namespace, roleService)
}
