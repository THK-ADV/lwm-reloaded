package modules

import controllers.{LabworkCRUDController, LabworkControllerPostgres}
import services.{LabworkService, LabworkServiceImpl}

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
  self: DatabaseModule with CourseServiceModule with UserServiceModule with DegreeServiceModule with SemesterServiceModule =>
  def labworkService: LabworkService
}

trait DefaultLabworkServiceModule extends LabworkServiceModule {
  self: DatabaseModule with CourseServiceModule with UserServiceModule with DegreeServiceModule with SemesterServiceModule =>
  override lazy val labworkService = new LabworkServiceImpl(db, courseService, userService, degreeService, semesterService)
}

trait LabworkManagementModulePostgres {
  def labworControllerPostgres: LabworkControllerPostgres
}


trait DefaultLabworkManagementModulePostgres extends LabworkManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with LabworkServiceModule with RoleServiceModule=>

  override lazy val labworControllerPostgres: LabworkControllerPostgres = new LabworkControllerPostgres(sessionService, roleService, labworkService)
}