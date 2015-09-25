package modules

import controllers.crud.CourseCRUDController
import services.RoleService

trait CourseManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, namespace, roleService)
}
