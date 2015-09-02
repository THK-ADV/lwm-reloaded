package modules

import java.util.UUID

import org.w3.banana.sesame.Sesame
import services.UsernameResolver
import store.Prefixes.LWMPrefix
import store.sparql._
import store.sparql.select._

trait UsernameResolverModule {
  self: SemanticRepositoryModule =>
  def resolver: UsernameResolver
}


trait DefaultUserResolverModule extends UsernameResolverModule {
  self: SemanticRepositoryModule =>
  override def resolver: UsernameResolver = new UsernameResolver {
    override def resolve(systemId: String): Option[UUID] = {
      val prefix = LWMPrefix[Sesame]
      repository.query {
        select("id") where {
          ^(v("s"), p(prefix.systemId), o(systemId)).
            ^(v("s"), p(prefix.id), v("id"))
        }
      }.flatMap(_.get("id")).map(v => UUID.fromString(v.stringValue()))
    }
  }
}
