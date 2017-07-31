package modules

import controllers.{LabworkCRUDController, LabworkControllerPostgres}
import dao.{LabworkService, LabworkServiceImpl}

trait LabworkManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait LabworkServiceModule {
  self: DatabaseModule =>

  def labworkService: LabworkService
}

trait DefaultLabworkServiceModule extends LabworkServiceModule {
  self: DatabaseModule =>

  override lazy val labworkService = new LabworkServiceImpl(db)
}

trait LabworkManagementModulePostgres {
  def labworControllerPostgres: LabworkControllerPostgres
}


trait DefaultLabworkManagementModulePostgres extends LabworkManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with LabworkServiceModule with RoleServiceModule=>

  override lazy val labworControllerPostgres: LabworkControllerPostgres = new LabworkControllerPostgres(sessionService, roleService, labworkService)
}