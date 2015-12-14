package modules.store

import store.{LwmResolvers, Resolvers}

trait ResolversModule {
  self: SemanticRepositoryModule =>
  def resolver: Resolvers
}


trait DefaultResolversModule extends ResolversModule {
  self: SemanticRepositoryModule =>
  override def resolver: Resolvers = new LwmResolvers(repository)
}
