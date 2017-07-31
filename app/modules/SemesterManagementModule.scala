package modules

import controllers.{SemesterCRUDController, SemesterControllerPostgres}
import dao.{SemesterDao, SemesterDaoImpl}

trait SemesterManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def semesterManagementController: SemesterCRUDController
}

trait DefaultSemesterManagementModuleImpl extends SemesterManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val semesterManagementController: SemesterCRUDController = new SemesterCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait SemesterDaoModule { self: DatabaseModule =>
  def semesterDao: SemesterDao
}

trait DefaultSemesterDaoModule extends SemesterDaoModule { self: DatabaseModule =>
  override lazy val semesterDao = new SemesterDaoImpl(db)
}

trait SemesterManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with SemesterDaoModule =>

  def semesterManagementControllerPostgres: SemesterControllerPostgres
}

trait DefaultSemesterManagementModuleImplPostgres extends SemesterManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with SemesterDaoModule =>

  lazy val semesterManagementControllerPostgres: SemesterControllerPostgres = new SemesterControllerPostgres(sessionService, roleService, semesterDao)
}