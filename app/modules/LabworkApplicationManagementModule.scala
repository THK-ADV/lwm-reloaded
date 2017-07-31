package modules

import controllers.{LabworkApplicationCRUDController, LabworkApplicationControllerPostgres}
import dao.{LabworkApplicationService2, LabworkApplicationServiceImpl}
import services.{LabworkApplicationService, LabworkApplicationServiceLike}
import utils.LwmApplication

trait LabworkApplicationServiceModule {
  self: LwmApplication with SemanticRepositoryModule =>

  def labworkApplicationService: LabworkApplicationServiceLike
}

trait DefaultLabworkApplicationServiceModule extends LabworkApplicationServiceModule {
  self: LwmApplication with SemanticRepositoryModule =>

  override lazy val labworkApplicationService: LabworkApplicationServiceLike = LabworkApplicationService(repository)
}

trait LabworkApplicationManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def labworkApplicationController: LabworkApplicationCRUDController
}

trait DefaultLabworkApplicationManagementModule extends LabworkApplicationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val labworkApplicationController: LabworkApplicationCRUDController = new LabworkApplicationCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait LabworkApplication2ServiceModule { self: DatabaseModule =>
  def labworkApplicationService2: LabworkApplicationService2
}

trait DefaultLabworkApplication2ServiceModule extends LabworkApplication2ServiceModule { self: DatabaseModule =>
  override lazy val labworkApplicationService2 = new LabworkApplicationServiceImpl(db)
}

trait LabworkApplicationManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with LabworkApplication2ServiceModule =>

  def labworkApplicationControllerPostgres: LabworkApplicationControllerPostgres
}

trait DefaultLabworkApplicationManagementModulePostgres extends LabworkApplicationManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with LabworkApplication2ServiceModule =>

  override lazy val labworkApplicationControllerPostgres: LabworkApplicationControllerPostgres = new LabworkApplicationControllerPostgres(sessionService, roleService, labworkApplicationService2)
}