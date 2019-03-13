package di

import com.typesafe.config.Config
import javax.inject.{Inject, Provider, Singleton}
import slick.jdbc.PostgresProfile.api._

@Singleton
class DatabaseProvider @Inject()(config: Config) extends Provider[Database] {
  lazy val get = Database.forConfig("database", config)
}
