package dao

import dao.helper._
import database.UniqueTable
import models.{UniqueDbEntity, UniqueEntity}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future

trait AbstractDao[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity]
  extends Core
    with Accessible[T, DbModel]
    with Expandable[DbModel]
    with Added[T, DbModel]
    with Invalidated[T, DbModel]
    with Updated[T, DbModel]
    with Retrieved[T, DbModel, LwmModel] {

  final def transaction[R](args: DBIO[R]*): Future[Unit] = db.run(DBIO.seq(args: _*).transactionally)

  final def createSchema: Future[Unit] = db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)

  final def dropSchema: Future[Unit] = db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)

  def close = Future.successful(db.close())
}