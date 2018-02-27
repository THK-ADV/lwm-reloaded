package modules

import controllers.AssignmentPlanControllerPostgres
import dao.{AssignmentPlanDao, AssignmentPlanDaoImpl}

trait AssignmentPlanDaoModule {
  self: DatabaseModule =>

  def assignmentPlanDao: AssignmentPlanDao
}

trait DefaultAssignmentPlanDaoModule extends AssignmentPlanDaoModule {
  self: DatabaseModule =>

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