package di

import javax.inject.{Provider, Singleton}
import slick.jdbc.JdbcProfile

@Singleton
class JdbcProfileProvider extends Provider[JdbcProfile] {
  override lazy val get = slick.jdbc.PostgresProfile
}
