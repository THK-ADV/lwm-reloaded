package modules

import controllers.{AssignmentPlanCRUDController, AssignmentPlanControllerPostgres}
import dao.{AssignmentPlanDao, AssignmentPlanDaoImpl}

trait AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def assignmentPlanManagementController: AssignmentPlanCRUDController
}

trait DefaultAssignmentPlanManagementModuleImpl extends AssignmentPlanManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val assignmentPlanManagementController: AssignmentPlanCRUDController = new AssignmentPlanCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait AssignmentPlanDaoModule { self: DatabaseModule =>
  def assignmentPlanDao: AssignmentPlanDao
}

trait DefaultAssignmentPlanDaoModule extends AssignmentPlanDaoModule { self: DatabaseModule =>
  override lazy val assignmentPlanDao = new AssignmentPlanDaoImpl(db)
}

trait AssignmentPlanManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with AssignmentPlanDaoModule =>

  def assignmentPlanManagementControllerPostgres: AssignmentPlanControllerPostgres
}

trait DefaultAssignmentPlanManagementModuleImplPostgres extends AssignmentPlanManagementModulePostgres {
  self: AuthorityDaoModule with SessionRepositoryModule with AssignmentPlanDaoModule =>

  lazy val assignmentPlanManagementControllerPostgres: AssignmentPlanControllerPostgres = new AssignmentPlanControllerPostgres(sessionService, authorityDao, assignmentPlanDao)
}