package dao

import java.sql.Timestamp
import java.util.UUID

import database.{TableFilter, UniqueTable}
import models.{UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import slick.dbio.Effect
import slick.dbio.Effect.Write
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep
import slick.sql.FixedSqlAction
import utils.LwmDateTime._

import scala.concurrent.Future
import scala.util.Try

trait DatabaseExpander[DbModel <: UniqueDbEntity] {
  def expandCreationOf[E <: Effect](entities: Seq[DbModel]): DBIOAction[Seq[DbModel], NoStream, Write with E]

  def expandUpdateOf(entity: DbModel): DBIOAction[DbModel, NoStream, Write]

  def expandDeleteOf(entity: DbModel): DBIOAction[DbModel, NoStream, Write]
}

case class ModelAlreadyExists[A](value: A) extends Throwable {
  override def getMessage = s"model already exists $value"
}

trait AbstractDao[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {

  import scala.concurrent.ExecutionContext.Implicits.global

  case class IdFilter(value: String) extends TableFilter[T] {
    override def predicate = _.id === UUID.fromString(value)
  }

  protected def db: PostgresProfile.backend.Database

  def tableQuery: TableQuery[T]

  protected def toAtomic(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def toUniqueEntity(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def existsQuery(entity: DbModel): Query[T, DbModel, Seq]

  protected def shouldUpdate(existing: DbModel, toUpdate: DbModel): Boolean

  protected def databaseExpander: Option[DatabaseExpander[DbModel]] = None

  final def transaction[R](args: DBIO[R]*): Future[Unit] = db.run(DBIO.seq(args: _*).transactionally)

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

  final def filterBy(tableFilter: List[TableFilter[T]], validOnly: Boolean = true, sinceLastModified: Option[String] = None): Query[T, DbModel, Seq] = {
    val query = tableFilter match {
      case h :: t =>
        t.foldLeft(tableQuery.filter(h.predicate)) { (query, nextFilter) =>
          query.filter(nextFilter.predicate)
        }
      case _ => tableQuery
    }

    filterBy(validOnly, sinceLastModified, query)
  }

  private def filterBy(validOnly: Boolean, sinceLastModified: Option[String], query: Query[T, DbModel, Seq]) = {
    val lastModified = sinceLastModified.fold(query)(t => query.filter(_.lastModifiedSince(new Timestamp(t.toLong))))

    if (validOnly) lastModified.filter(_.isValid) else lastModified
  }

  final def get(tableFilter: List[TableFilter[T]] = List.empty, atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Seq[LwmModel]] = {
    val query = filterBy(tableFilter, validOnly, sinceLastModified)

    if (atomic) toAtomic(query) else toUniqueEntity(query)
  }

  // TODO refactor get functions... they are pretty messy right now

  final def filter(where: T => Rep[Boolean], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None) = {
    val query = filterBy(validOnly, sinceLastModified, tableQuery.filter(where))

    if (atomic) toAtomic(query) else toUniqueEntity(query)
  }

  final def getMany(ids: List[UUID], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Seq[LwmModel]] = {
    val query = filterBy(validOnly, sinceLastModified, tableQuery.filter(_.id.inSet(ids)))

    if (atomic) toAtomic(query) else toUniqueEntity(query)
  }

  // TODO use this function instead of get(tableFilter) for a single entity
  final def getById(id: String, atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Option[LwmModel]] = {
    getMany(List(UUID.fromString(id)), atomic, validOnly, sinceLastModified).map(_.headOption)
  }

  final def delete(entity: DbModel): Future[Timestamp] = delete(entity.id)

  final def delete(id: UUID): Future[Timestamp] = db.run(deleteExpandableQuery(id))

  final def deleteManyEntities(entities: List[DbModel]): Future[List[Timestamp]] = deleteMany(entities.map(_.id))

  final def deleteMany(ids: List[UUID]): Future[List[Timestamp]] = {
    val query = ids.map(deleteExpandableQuery)
    db.run(DBIO.sequence(query))
  }

  protected final def deleteExpandableQuery(id: UUID): DBIOAction[Timestamp, NoStream, Effect.Read with Write with Effect.Transactional] = {
    val query = deleteQuery(id)

    databaseExpander.fold(query) { expander =>
      (for {
        q <- query
        e <- expander.expandDeleteOf(q)
      } yield e).transactionally
    }.map(_.lastModified)
  }

  protected final def deleteQuery(id: UUID, now: Timestamp = DateTime.now.timestamp): DBIOAction[DbModel, NoStream, Effect.Read with Write with Effect.Transactional] = {
    val found = tableQuery.filter(_.id === id)

    for {
      existing <- found.result if existing.nonEmpty
      _ <- found.map(f => (f.lastModified, f.invalidated)).update((now, Some(now)))
    } yield existing.head
  }

  final def update(entity: DbModel): Future[DbModel] = db.run(updateExpandableQuery(entity))

  final def updateMany(entities: List[DbModel]): Future[List[DbModel]] = {
    val query = entities.map(updateExpandableQuery)
    db.run(DBIO.sequence(query))
  }

  protected final def updateQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write with Write with Effect.Transactional] = {
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

  protected final def updateExpandableQuery(entity: DbModel): DBIOAction[DbModel, NoStream, Effect.Read with Write with Write with Effect.Transactional] = {
    val query = updateQuery(entity)

    databaseExpander.fold(query) { expander =>
      (for {
        q <- query
        e <- expander.expandUpdateOf(q)
      } yield e).transactionally
    }
  }

  def createSchema: Future[Unit] = db.run(tableQuery.schema.create)

  def dropSchema: Future[Unit] = db.run(tableQuery.schema.create)
}
