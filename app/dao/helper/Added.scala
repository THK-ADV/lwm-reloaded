package dao.helper

import database.UniqueTable
import models.UniqueDbEntity
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Future
import scala.util.Try

trait Added[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity] {
  self: Core with Expandable[DbModel] with Accessible[T, DbModel] =>

  protected def existsQuery(entity: DbModel): Query[T, DbModel, Seq]

  final def create(entity: DbModel): Future[DbModel] = db run createQuery(entity)

  final def createMany(entities: List[DbModel]): Future[Seq[DbModel]] = db run createManyQuery(entities)

  final def createManyPartial(entities: List[DbModel]): Future[List[Try[DbModel]]] = {
    import utils.Ops.FutureOps
    entities.map(create).asTrys
  }

  final def createOrUpdateMany(entities: Seq[DbModel]): Future[Seq[DbModel]] = {
    val query = entities.map(e => (tableQuery returning tableQuery).insertOrUpdate(e))
    val action = DBIO.sequence(query)

    db.run(action.transactionally).map(_ => entities)
  }

  def createQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    val singleQuery = for {
      existing <- existsQuery(entity).result
      created <- if (existing.nonEmpty)
        DBIO.failed(ModelAlreadyExists(entity, existing))
      else
        (tableQuery returning tableQuery) += entity
    } yield created

    val expandedQuery = databaseExpander.fold {
      singleQuery
    } { expander =>
      for {
        _ <- singleQuery
        e <- expander.expandCreationOf(entity)
      } yield e.head
    }

    expandedQuery.transactionally
  }

  final def createManyQuery(entities: Seq[DbModel]): DBIOAction[Seq[DbModel], NoStream, Effect.Read with Write with Effect.Transactional] = {
    DBIO.sequence(entities map createQuery)
  }
}
