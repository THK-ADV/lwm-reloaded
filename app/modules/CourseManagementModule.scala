package modules

import controllers.{CourseCRUDController, CourseControllerPostgres}
import services.CourseService

trait CourseManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, sessionService, namespace, roleService)
}

trait CourseManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule  =>

  def courseManagementControllerPostgres: CourseControllerPostgres
}

trait DefaultCourseManagementModuleImplPostgres extends CourseManagementModulePostgres {
  self: SecurityManagementModule with SessionRepositoryModule  =>

  lazy val courseManagementControllerPostgres: CourseControllerPostgres = new CourseControllerPostgres(sessionService, roleService, CourseService)
}
