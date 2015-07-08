package modules

import play.api.Play
import store.{Namespace, SesameRepository}


trait BaseNamespace {
  def namespace: Namespace
}

trait ConfigurableBaseNamespace extends BaseNamespace {
  self: ConfigurationModule =>
  lwmConfig.underlying.resolve()
  override def namespace: Namespace = Namespace(lwmConfig.underlying.getString("lwm.namespace"))
}

trait SemanticRepositoryModule {
  self: BaseNamespace =>
  def repository: SesameRepository
}

trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  self: BaseNamespace =>

  def repository: SesameRepository = SesameRepository(namespace)
}
