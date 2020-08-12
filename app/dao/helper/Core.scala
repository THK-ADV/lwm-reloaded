package dao.helper

import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext

trait Core {
  def db: Database

  protected implicit def executionContext: ExecutionContext
}
