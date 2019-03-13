package dao.helper

import database.UniqueTable
import models.UniqueDbEntity
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._

trait Accessible[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  protected def tableQuery: TableQuery[T]

  protected def schemas: List[PostgresProfile.SchemaDescription] = List(tableQuery.schema)
}
