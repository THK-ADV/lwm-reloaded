package modules

import controllers.crud.CourseCRUDController
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}
import services.RoleService

trait CourseManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule =>

  def courseManagementController: CourseCRUDController
}

trait DefaultCourseManagementModuleImpl extends CourseManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule =>
  lazy val courseManagementController: CourseCRUDController = new CourseCRUDController(repository, namespace, roleService)
}
