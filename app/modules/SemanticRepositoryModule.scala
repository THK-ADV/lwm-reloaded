package modules

import store.{Namespace, SesameRepository}


trait SemanticRepositoryModule {
  def repository: SesameRepository

  def namespace: Namespace
}

trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  def namespace: Namespace = Namespace("http://lwm/")

  def repository: SesameRepository = SesameRepository(namespace)
}
