package modules

import controllers.PermissionController
import services.{PermissionService, PermissionServiceImpl}

trait PermissionManagementModule {
  self: SemanticRepositoryModule with SecurityManagementModule with SessionRepositoryModule =>

  def permissionManagementController: PermissionController
}

trait DefaultPermissionManagementModule extends PermissionManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  override lazy val permissionManagementController: PermissionController = new PermissionController(repository, sessionService, namespace, roleService)
}

// POSTGRES

trait PermissionServiceModule { self: DatabaseModule =>
  def permissionService: PermissionService
}

trait DefaultPermissionServiceModule extends PermissionServiceModule { self: DatabaseModule =>
  override lazy val permissionService = new PermissionServiceImpl(db)
}