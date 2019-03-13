package di

import javax.inject.Inject
import play.api.inject.ApplicationLifecycle
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

/** Closes database connections safely.  Important on dev restart. */
class DatabaseCloseHook @Inject()(val database: Database, lifecycle: ApplicationLifecycle) {
  lifecycle.addStopHook { () =>
    Future.successful(database.close()) // TODO ensure that there is only one instance of Database
  }
}
