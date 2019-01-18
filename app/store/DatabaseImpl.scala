package store

import javax.inject.{Inject, Singleton}
import slick.jdbc.PostgresProfile

//trait Database {
//  def db: PostgresProfile.backend.Database
//}
//
//@Singleton
//class DatabaseImpl @Inject()(val db: PostgresProfile.backend.Database) extends Database