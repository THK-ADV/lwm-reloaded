package store

import java.util.UUID

import org.w3.banana.sesame.Sesame
import store.Prefixes.LWMPrefix
import store.sparql.select
import store.sparql.select._

trait UsernameResolver {
  def resolve(systemId: String): Option[UUID]
}

class LwmUsernameResolver(val repository: SesameRepository) extends UsernameResolver {

  override def resolve(systemId: String): Option[UUID] = {
    val prefix = LWMPrefix[Sesame]
    repository.query {
      select("id") where {
        ^(v("s"), p(prefix.systemId), o(systemId)).
          ^(v("s"), p(prefix.id), o("id"))
      }
    }.flatMap(_.map(value => UUID.fromString(value.stringValue())).headOption)
  }
}
