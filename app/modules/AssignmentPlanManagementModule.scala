package modules

import controllers.{AssignmentPlanCRUDController, AssignmentPlanControllerPostgres}
import services.{AssignmentPlanService, AssignmentPlanServiceImpl}

trait AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def assignmentPlanManagementController: AssignmentPlanCRUDController
}

trait DefaultAssignmentPlanManagementModuleImpl extends AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val assignmentPlanManagementController: AssignmentPlanCRUDController = new AssignmentPlanCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait AssignmentPlanServiceModule { self: DatabaseModule =>
  def assignmentPlanService: AssignmentPlanService
}

trait DefaultAssignmentPlanServiceModule extends AssignmentPlanServiceModule{ self: DatabaseModule =>
  override lazy val assignmentPlanService = new AssignmentPlanServiceImpl(db)
}

trait AssignmentPlanManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with AssignmentPlanServiceModule=>

  def assignmentPlanManagementControllerPostgres: AssignmentPlanControllerPostgres
}

trait DefaultAssignmentPlanManagementModuleImplPostgres extends AssignmentPlanManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with AssignmentPlanServiceModule =>

  lazy val assignmentPlanManagementControllerPostgres: AssignmentPlanControllerPostgres = new AssignmentPlanControllerPostgres(sessionService, roleService, assignmentPlanService)
}