package modules

import controllers.crud.CourseCRUDController
import services.RoleService

trait CourseManagementModule {
  self: SemanticRepositoryModule with RoleManagementModule =>

  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with RoleManagementModule =>
  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, namespace, roleService)
}
