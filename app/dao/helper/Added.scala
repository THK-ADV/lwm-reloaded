package dao.helper

import database.UniqueTable
import models.UniqueDbEntity
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._
import slick.sql.FixedSqlAction

import scala.concurrent.Future
import scala.util.Try

trait Added[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core[T, DbModel] =>

  protected def existsQuery(entity: DbModel): Query[T, DbModel, Seq]

  final def create(entity: DbModel): Future[DbModel] = {
    val query = createQuery(entity)

    databaseExpander.fold {
      db.run(query)
    } { expander =>
      db.run((for {
        _ <- query
        e <- expander.expandCreationOf(List(entity))
      } yield e.head).transactionally)
    }
  }

  final def createQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write] = {
    existsQuery(entity).result.flatMap { exists =>
      if (exists.nonEmpty)
        DBIO.failed(ModelAlreadyExists(exists))
      else
        (tableQuery returning tableQuery) += entity
    }
  }

  final def createMany(entities: List[DbModel]): Future[Seq[DbModel]] = {
    databaseExpander.fold {
      db.run(createManyQuery(entities))
    } { expander =>
      db.run((for {
        _ <- createManyQuery(entities)
        e <- expander.expandCreationOf(entities)
      } yield e).transactionally)
    }
  }

  final def createManyPartial(entities: List[DbModel]): Future[List[Try[DbModel]]] = {
    import utils.Ops.FutureOps
    entities.map(create).asTrys
  }

  final def createManyQuery(entities: Seq[DbModel]): FixedSqlAction[Seq[DbModel], NoStream, Write] = (tableQuery returning tableQuery) ++= entities

  final def createOrUpdateMany(entities: Seq[DbModel]): Future[Seq[DbModel]] = {
    val query = entities.map(e => (tableQuery returning tableQuery).insertOrUpdate(e))
    val action = DBIO.seq(query: _*)

    db.run(action.transactionally).map(_ => entities)
  }
}
