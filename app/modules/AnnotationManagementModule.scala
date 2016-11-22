package modules

import controllers.AnnotationCRUDController

trait AnnotationManagementModule {

  def annotationManagementController: AnnotationCRUDController
}

trait DefaultAnnotationManagementModuleImpl extends AnnotationManagementModule {
  self: SemanticRepositoryModule with BaseNamespace with SecurityManagementModule with SessionRepositoryModule =>

  lazy val annotationManagementController: AnnotationCRUDController = new AnnotationCRUDController(repository, sessionService, namespace, roleService)
}