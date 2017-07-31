package modules

import controllers.{SemesterCRUDController, SemesterControllerPostgres}
import dao.{SemesterService, SemesterServiceImpl}

trait SemesterManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait SemesterServiceModule { self: DatabaseModule =>
  def semesterService: SemesterService
}

trait DefaultSemesterServiceModule extends SemesterServiceModule { self: DatabaseModule =>
  override lazy val semesterService = new SemesterServiceImpl(db)
}

trait SemesterManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with SemesterServiceModule =>

  def semesterManagementControllerPostgres: SemesterControllerPostgres
}

trait DefaultSemesterManagementModuleImplPostgres extends SemesterManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with SemesterServiceModule =>

  lazy val semesterManagementControllerPostgres: SemesterControllerPostgres = new SemesterControllerPostgres(sessionService, roleService, semesterService)
}