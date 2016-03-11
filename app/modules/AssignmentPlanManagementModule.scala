package modules

import controllers.crud.AssignmentPlanCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait AssignmentPlanManagementModule {self =>

  def assignmentPlanManagementController: AssignmentPlanCRUDController
}

trait DefaultAssignmentPlanManagementModuleImpl extends AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>

  lazy val assignmentPlanManagementController: AssignmentPlanCRUDController = new AssignmentPlanCRUDController(repository, namespace, roleService)
}