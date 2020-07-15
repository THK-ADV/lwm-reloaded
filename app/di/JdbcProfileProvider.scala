package di

import com.google.inject.Provider
import javax.inject.Singleton
import slick.jdbc.JdbcProfile

@Singleton
class JdbcProfileProvider extends Provider[JdbcProfile] {
  override lazy val get = slick.jdbc.PostgresProfile
}
