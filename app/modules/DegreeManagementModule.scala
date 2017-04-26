package modules

import controllers.{DegreeCRUDController, DegreeControllerPostgres}
import services.DegreeService

trait DegreeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, sessionService, namespace, roleService)
}

trait DegreeManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>
  def degreeManagementControllerPostgres: DegreeControllerPostgres
}

trait DefaultDegreeManagementModuleImplPostgres extends DegreeManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  lazy val degreeManagementControllerPostgres: DegreeControllerPostgres = new DegreeControllerPostgres(sessionService, roleService, DegreeService)
}
