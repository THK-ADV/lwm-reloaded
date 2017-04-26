package modules

import controllers.{AssignmentPlanCRUDController, AssignmentPlanControllerPostgres}
import services.AssignmentPlanService

trait AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def assignmentPlanManagementController: AssignmentPlanCRUDController
}

trait DefaultAssignmentPlanManagementModuleImpl extends AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val assignmentPlanManagementController: AssignmentPlanCRUDController = new AssignmentPlanCRUDController(repository, sessionService, namespace, roleService)
}

trait AssignmentPlanManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  def assignmentPlanManagementControllerPostgres: AssignmentPlanControllerPostgres
}

trait DefaultAssignmentPlanManagementModuleImplPostgres extends AssignmentPlanManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  lazy val assignmentPlanManagementControllerPostgres: AssignmentPlanControllerPostgres = new AssignmentPlanControllerPostgres(sessionService, roleService, AssignmentPlanService)
}