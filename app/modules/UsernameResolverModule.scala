package modules

import java.util.UUID

import services.UsernameResolver

trait UsernameResolverModule { self: SemanticRepositoryModule =>
  def resolver: UsernameResolver
}


trait DefaultUserResolverModule extends UsernameResolverModule { self: SemanticRepositoryModule =>
  override def resolver: UsernameResolver = new UsernameResolver {
    override def resolve(systemId: String): Option[UUID] = ???
  }
}
