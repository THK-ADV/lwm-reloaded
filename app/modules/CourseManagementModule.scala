package modules

import controllers.{CourseCRUDController, CourseControllerPostgres}
import dao.{CourseService, CourseServiceImpl}

trait CourseManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait CourseServiceModule { self: DatabaseModule =>
  def courseService: CourseService
}

trait DefaultCourseServiceModule extends CourseServiceModule {
  self: DatabaseModule with AuthorityServiceModule =>

  override lazy val courseService = new CourseServiceImpl(db, authorityService)
}

trait CourseManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule  =>

  def courseManagementControllerPostgres: CourseControllerPostgres
}

trait DefaultCourseManagementModuleImplPostgres extends CourseManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule with CourseServiceModule with AuthorityServiceModule =>

  lazy val courseManagementControllerPostgres: CourseControllerPostgres = new CourseControllerPostgres(sessionService, roleService, courseService, authorityService)
}