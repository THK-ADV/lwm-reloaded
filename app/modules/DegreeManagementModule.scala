package modules

import controllers.{DegreeCRUDController, DegreeControllerPostgres}
import dao.{DegreeService, DegreeServiceImpl}

trait DegreeManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>
  def degreeManagementController: DegreeCRUDController
}

trait DefaultDegreeManagementModuleImpl extends DegreeManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val degreeManagementController: DegreeCRUDController = new DegreeCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait DegreeServiceModule { self: DatabaseModule =>
  def degreeService: DegreeService
}

trait DefaultDegreeServiceModule extends DegreeServiceModule { self: DatabaseModule =>
  override lazy val degreeService = new DegreeServiceImpl(db)
}

trait DegreeManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with DegreeServiceModule =>
  def degreeManagementControllerPostgres: DegreeControllerPostgres
}

trait DefaultDegreeManagementModuleImplPostgres extends DegreeManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with DegreeServiceModule =>

  lazy val degreeManagementControllerPostgres: DegreeControllerPostgres = new DegreeControllerPostgres(sessionService, roleService, degreeService)
}
