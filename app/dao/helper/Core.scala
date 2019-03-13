package dao.helper

import database.UniqueTable
import models.UniqueDbEntity
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext

trait Core[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  protected def db: PostgresProfile.backend.Database

  protected def tableQuery: TableQuery[T]

  protected def databaseExpander: Option[DatabaseExpander[DbModel]] = None

  protected implicit def executionContext: ExecutionContext
}
