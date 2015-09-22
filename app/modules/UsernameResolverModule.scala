package modules

import store.{UsernameResolver, LwmUsernameResolver}

trait UsernameResolverModule {
  self: SemanticRepositoryModule =>
  def resolver: UsernameResolver
}


trait DefaultUserResolverModule extends UsernameResolverModule {
  self: SemanticRepositoryModule =>
  override def resolver: UsernameResolver = new LwmUsernameResolver(repository)
}
