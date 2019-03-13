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
  self: Core with Expandable[DbModel] with Accessible[T, DbModel] =>

  protected def shouldUpdate(existing: DbModel, toUpdate: DbModel): Boolean

  final def update(entity: DbModel): Future[DbModel] = db.run(updateQuery(entity))

  final def updateMany(entities: List[DbModel]): Future[List[DbModel]] = {
    val query = entities.map(updateQuery)
    db.run(DBIO.sequence(query))
  }

  final def updateQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write with Write with Effect.Transactional] = {
    val found = tableQuery.filter(_.id === entity.id)

    val singleQuery = found.result.head.flatMap { existing =>
      if (shouldUpdate(existing, entity))
        (for {
          u1 <- found.update(entity)
          u2 <- found.map(_.lastModified).update(DateTime.now.timestamp)
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
