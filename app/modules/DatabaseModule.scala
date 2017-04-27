package modules

import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._

trait DatabaseModule {
  def db: PostgresDriver.backend.Database
}

trait DefaultDatabaseModule extends DatabaseModule {
  override lazy val db = Database.forConfig("database")
}
