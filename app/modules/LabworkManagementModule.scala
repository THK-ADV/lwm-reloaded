package modules

import controllers.{LabworkCRUDController, LabworkControllerPostgres}
import dao.{LabworkDao, LabworkDaoImpl}

trait LabworkManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def labworkManagementController: LabworkCRUDController
}


trait DefaultLabworkManagementModuleImpl extends LabworkManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val labworkManagementController: LabworkCRUDController = new LabworkCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait LabworkDaoModule { self: DatabaseModule =>
  def labworkDao: LabworkDao
}

trait DefaultLabworkDaoModule extends LabworkDaoModule { self: DatabaseModule =>
  override lazy val labworkDao = new LabworkDaoImpl(db)
}

trait LabworkManagementModulePostgres {
  def labworControllerPostgres: LabworkControllerPostgres
}

trait DefaultLabworkManagementModulePostgres extends LabworkManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with LabworkDaoModule with RoleDaoModule=>

  override lazy val labworControllerPostgres: LabworkControllerPostgres = new LabworkControllerPostgres(sessionService, roleService, labworkDao)
}