package modules.labwork

import controllers.crud.labwork.AnnotationCRUDController
import modules.SessionRepositoryModule
import modules.security.SecurityManagementModule
import modules.store.{BaseNamespace, SemanticRepositoryModule}

trait AnnotationManagementModule {

  def annotationManagementController: AnnotationCRUDController
}

trait DefaultAnnotationManagementModuleImpl extends AnnotationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val annotationManagementController: AnnotationCRUDController = new AnnotationCRUDController(repository, sessionService, namespace, roleService)
}