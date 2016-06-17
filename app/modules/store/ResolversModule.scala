package modules.store

import store.{LwmResolvers, Resolvers}

trait ResolversModule {
  self: SemanticRepositoryModule =>

  def resolvers: Resolvers
}


trait DefaultResolversModule extends ResolversModule {
  self: SemanticRepositoryModule =>

  override def resolvers: Resolvers = new LwmResolvers(repository)
}
