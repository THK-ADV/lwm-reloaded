package modules.labwork

import controllers.crud.labwork.AssignmentPlanCRUDController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def assignmentPlanManagementController: AssignmentPlanCRUDController
}

trait DefaultAssignmentPlanManagementModuleImpl extends AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val assignmentPlanManagementController: AssignmentPlanCRUDController = new AssignmentPlanCRUDController(repository, sessionService, namespace, roleService)
}