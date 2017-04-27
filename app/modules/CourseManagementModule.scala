package modules

import controllers.CourseCRUDController
import services.{CourseService, CourseServiceImpl}

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

trait DefaultCourseServiceModule extends CourseServiceModule { self: DatabaseModule =>
  override lazy val courseService = new CourseServiceImpl(db)
}