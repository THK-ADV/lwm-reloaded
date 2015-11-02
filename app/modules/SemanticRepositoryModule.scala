package modules

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
//TODO: ADD A BLOODY FOLDER FOR STORAGE!!!
trait DefaultSemanticRepositoryModuleImpl extends SemanticRepositoryModule {
  self: BaseNamespace =>
  val repository: SesameRepository = SesameRepository(namespace)
}
