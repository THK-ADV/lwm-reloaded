package dao.helper

import java.sql.Timestamp
import java.util.UUID

import database.UniqueTable
import models.UniqueDbEntity
import org.joda.time.DateTime
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

import scala.concurrent.Future

trait Removed[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core
    with Expandable[DbModel]
    with Accessible[T, DbModel]
    with Retrieved[T, DbModel, _] =>

  final def delete(entity: DbModel): Future[DbModel] = delete(entity.id)

  final def delete(id: UUID): Future[DbModel] = db.run(deleteSingle(id))

  final def deleteManyEntities(entities: List[DbModel]): Future[List[DbModel]] = deleteMany(entities.map(_.id))

  final def deleteMany(ids: List[UUID]): Future[List[DbModel]] = {
    val query = ids.map(id => deleteSingle(id))
    db.run(DBIO.sequence(query))
  }

  final def deleteSingle(id: UUID, now: Timestamp = DateTime.now.timestamp) = {
    deleteSingle0(filterValidOnly(_.id === id), now)
  }

  final def deleteSingleWhere(where: T => Rep[Boolean], now: Timestamp = DateTime.now.timestamp) = {
    deleteSingle0(filterValidOnly(where), now)
  }

  final def deleteSingleQuery(query: Query[T, DbModel, Seq], now: Timestamp = DateTime.now.timestamp) = {
    deleteSingle0(query, now)
  }

  private def deleteSingle0(query: Query[T, DbModel, Seq], now: Timestamp) = {
    val singleQuery = query.exactlyOne { toDelete =>
      for {
        _ <- query.map(f => (f.lastModified, f.invalidated)).update((now, Some(now)))
      } yield toDelete
    }

    val expandableQuery = databaseExpander.fold {
      singleQuery
    } { expander =>
      for {
        q <- singleQuery
        e <- expander.expandDeleteOf(q)
      } yield e
    }

    expandableQuery.transactionally
  }
}
