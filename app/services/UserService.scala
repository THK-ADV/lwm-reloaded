package services

import models._
import slick.driver.PostgresDriver.api._
import store.UserTable

import scala.concurrent.Future

trait UserService extends AbstractDao[UserTable, DbUser] {
  override protected def tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  def filter(status: String): Future[Seq[DbUser]] = filter(_.status === status)
}

object UserService extends UserService