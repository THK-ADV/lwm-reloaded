package dao.helper

import database.UniqueTable
import models.UniqueDbEntity
import org.joda.time.DateTime
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

import scala.concurrent.Future

trait Updated[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core[T, DbModel] =>

  protected def shouldUpdate(existing: DbModel, toUpdate: DbModel): Boolean

  final def update(entity: DbModel): Future[DbModel] = db.run(updateExpandableQuery(entity))

  final def updateMany(entities: List[DbModel]): Future[List[DbModel]] = {
    val query = entities.map(updateExpandableQuery)
    db.run(DBIO.sequence(query))
  }

  final def updateQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write with Write with Effect.Transactional] = {
    val found = tableQuery.filter(_.id === entity.id)

    found.result.head.flatMap { existing =>
      if (shouldUpdate(existing, entity))
        (for {
          u1 <- found.update(entity)
          u2 <- found.map(_.lastModified).update(DateTime.now.timestamp)
        } yield u1 + u2).transactionally.map(_ => entity)
      else
        DBIO.failed(ModelAlreadyExists(existing))
    }
  }

  final def updateExpandableQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write with Write with Effect.Transactional] = {
    val query = updateQuery(entity)

    databaseExpander.fold(query) { expander =>
      (for {
        q <- query
        e <- expander.expandUpdateOf(q)
      } yield e).transactionally
    }
  }
}
