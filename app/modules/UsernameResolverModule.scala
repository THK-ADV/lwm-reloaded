package modules

import store.{LwmUsernameResolver, UsernameResolver}

trait UsernameResolverModule {
  self: SemanticRepositoryModule =>
  def resolver: UsernameResolver
}


trait DefaultUserResolverModule extends UsernameResolverModule {
  self: SemanticRepositoryModule =>
  override def resolver: UsernameResolver = new LwmUsernameResolver(repository)
}
