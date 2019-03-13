package dao

import java.util.UUID

import dao.helper._
import database.{TableFilter, UniqueTable}
import models.{UniqueDbEntity, UniqueEntity}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

trait AbstractDao[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
  extends Core[T, DbModel]
    with Added[T, DbModel]
    with Removed[T, DbModel]
    with Updated[T, DbModel]
    with Retrieved[T, DbModel, LwmModel] {

  case class IdFilter(value: String) extends TableFilter[T] {
    override def predicate: T => Rep[Boolean] = _.id === UUID.fromString(value)
  }

  final def transaction[R](args: DBIO[R]*): Future[Unit] = db.run(DBIO.seq(args: _*).transactionally)

  protected def createSchema: Future[Unit] = db.run(tableQuery.schema.create)

  protected def dropSchema: Future[Unit] = db.run(tableQuery.schema.create)
}