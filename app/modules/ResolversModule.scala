package modules

import store.{LwmResolvers, Resolvers}

trait ResolversModule {
  self: SemanticRepositoryModule =>

  def resolvers: Resolvers
}


trait DefaultResolversModule extends ResolversModule {
  self: SemanticRepositoryModule =>

  override lazy val resolvers: Resolvers = new LwmResolvers(repository)
}
