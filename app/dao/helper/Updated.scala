package dao.helper

import database.UniqueTable
import models.UniqueDbEntity
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

import scala.concurrent.Future

trait Updated[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core
    with Expandable[DbModel]
    with Accessible[T, DbModel]
    with Retrieved[T, DbModel, _] =>

  protected def shouldUpdate(existing: DbModel, toUpdate: DbModel): Boolean

  final def update(entity: DbModel): Future[DbModel] = db.run(updateQuery(entity))

  final def updateMany(entities: List[DbModel]): Future[List[DbModel]] = {
    val query = entities.map(updateQuery)
    db.run(DBIO.sequence(query))
  }

  final def updateQuery(entity: DbModel) = {
    val query = filterValidOnly(_.id === entity.id)

    val singleQuery = query.exactlyOne { existing =>
      if (shouldUpdate(existing, entity))
        (for {
          u1 <- query.update(entity)
          u2 <- query.map(_.lastModified).update(DateTime.now.timestamp)
        } yield u1 + u2).transactionally.map(_ => entity)
      else
        DBIO.failed(ModelAlreadyExists(existing))
    }

    databaseExpander.fold {
      singleQuery
    } { expander =>
      (for {
        q <- singleQuery
        e <- expander.expandUpdateOf(q)
      } yield e).transactionally
    }
  }
}
