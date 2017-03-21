package modules

import controllers.{DegreeCRUDController, DegreeCRUDControllerPostgres}
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
  def degreeManagementControllerPostgres: DegreeCRUDControllerPostgres
}

trait DefaultDegreeManagementModuleImplPostgres extends DegreeManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  lazy val degreeManagementControllerPostgres: DegreeCRUDControllerPostgres = new DegreeCRUDControllerPostgres(DegreeService, sessionService, roleService)
}
