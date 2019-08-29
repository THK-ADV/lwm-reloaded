package dao.helper

import java.sql.Timestamp
import java.util.UUID

import database.UniqueTable
import models.UniqueDbEntity
import org.joda.time.DateTime
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps.DateTimeConverter

import scala.concurrent.Future

trait Removed[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core with Expandable[DbModel] with Accessible[T, DbModel] =>

  final def delete(entity: DbModel): Future[DbModel] = delete(entity.id)

  final def delete(id: UUID): Future[DbModel] = db.run(deleteSingle(id))

  final def deleteManyEntities(entities: List[DbModel]): Future[List[DbModel]] = deleteMany(entities.map(_.id))

  final def deleteMany(ids: List[UUID]): Future[List[DbModel]] = {
    val query = ids.map(id => deleteSingle(id))
    db.run(DBIO.sequence(query))
  }

  final def deleteSingle(id: UUID, now: Timestamp = DateTime.now.timestamp): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    deleteSingle0(tableQuery.filter(_.id === id), now)
  }

  final def deleteSingleWhere(where: T => Rep[Boolean], now: Timestamp = DateTime.now.timestamp): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    deleteSingle0(tableQuery.filter(where), now)
  }

  final def deleteSingleQuery(query: PostgresProfile.api.Query[T, DbModel, Seq], now: Timestamp = DateTime.now.timestamp): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    deleteSingle0(query, now)
  }

  private def deleteSingle0(query: PostgresProfile.api.Query[T, DbModel, Seq], now: Timestamp): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    val singleQuery = for {
      existing <- query.result if existing.nonEmpty
      _ <- query.map(f => (f.lastModified, f.invalidated)).update((now, Some(now)))
    } yield existing.head

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
