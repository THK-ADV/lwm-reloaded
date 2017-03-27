package modules

import controllers.{SemesterCRUDController, SemesterControllerPostgres}
import services.SemesterService

trait SemesterManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, sessionService, namespace, roleService)
}

trait SemesterManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  def semesterManagementControllerPostgres: SemesterControllerPostgres
}

trait DefaultSemesterManagementModuleImplPostgres extends SemesterManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule =>

  lazy val semesterManagementControllerPostgres: SemesterControllerPostgres = new SemesterControllerPostgres(sessionService, roleService, SemesterService)
}