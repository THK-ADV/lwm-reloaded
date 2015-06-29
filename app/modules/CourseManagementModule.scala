package modules

import controllers.crud.CourseCRUDController

trait CourseManagementModule {
  self: SemanticRepositoryModule =>
  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule =>
  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, namespace)
}