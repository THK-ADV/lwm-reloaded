package modules

import store.{LwmResolvers, Resolvers}

trait UsernameResolverModule {
  self: SemanticRepositoryModule =>
  def resolver: Resolvers
}


trait DefaultUserResolverModule extends UsernameResolverModule {
  self: SemanticRepositoryModule =>
  override def resolver: Resolvers = new LwmResolvers(repository)
}
