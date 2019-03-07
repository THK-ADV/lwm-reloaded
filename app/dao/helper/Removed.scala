package dao.helper

import java.sql.Timestamp
import java.util.UUID

import database.UniqueTable
import models.UniqueDbEntity
import org.joda.time.DateTime
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime.DateTimeConverter

import scala.concurrent.Future

trait Removed[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core[T, DbModel] =>

  final def delete(entity: DbModel): Future[DbModel] = delete(entity.id)

  final def delete(id: UUID): Future[DbModel] = db.run(deleteQuery(id))

  final def deleteManyEntities(entities: List[DbModel]): Future[List[DbModel]] = deleteMany(entities.map(_.id))

  final def deleteMany(ids: List[UUID]): Future[List[DbModel]] = {
    val query = ids.map(id => deleteQuery(id))
    db.run(DBIO.sequence(query))
  }

  final def deleteQuery(id: UUID, now: Timestamp = DateTime.now.timestamp): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    val found = tableQuery.filter(_.id === id)

    val singleQuery = for {
      existing <- found.result if existing.nonEmpty
      _ <- found.map(f => (f.lastModified, f.invalidated)).update((now, Some(now)))
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
