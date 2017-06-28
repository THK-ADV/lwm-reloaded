package services

import java.sql.Timestamp
import java.util.UUID

import models.{UniqueDbEntity, UniqueEntity}
import org.joda.time.DateTime
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import slick.profile.FixedSqlAction
import store.{TableFilter, UniqueTable}
import models.LwmDateTime._
import scala.concurrent.Future

trait DatabaseExpander[DbModel <: UniqueDbEntity] {
  def expandCreationOf[E <: Effect](entities: Seq[DbModel]): DBIOAction[Seq[DbModel], NoStream, Write with E]
  def expandUpdateOf(entity: DbModel): DBIOAction[Option[DbModel], NoStream, Write]
  def expandDeleteOf(entity: DbModel): DBIOAction[Option[DbModel], NoStream, Write]
}

case class ModelAlreadyExists[A](value: A) extends Throwable {
  override def getMessage = s"model already exists $value"
}

trait AbstractDao[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  import scala.concurrent.ExecutionContext.Implicits.global

  case class IdFilter(value: String) extends TableFilter[T] {
    override def predicate = _.id === UUID.fromString(value)
  }

  protected def db: PostgresDriver.backend.Database

  def tableQuery: TableQuery[T]

  protected def toAtomic(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def toUniqueEntity(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def existsQuery(entity: DbModel): Query[T, DbModel, Seq]

  protected def shouldUpdate(existing: DbModel, toUpdate: DbModel): Boolean

  protected def databaseExpander: Option[DatabaseExpander[DbModel]] = None

  final def create(entity: DbModel): Future[DbModel] = {
    val query = existsQuery(entity).result.flatMap { exists =>
      if (exists.nonEmpty)
        DBIO.failed(ModelAlreadyExists(exists))
      else
        (tableQuery returning tableQuery) += entity
    }

    databaseExpander.fold {
      db.run(query)
    } { expander =>
      db.run((for {
        _ <- query
        e <- expander.expandCreationOf(List(entity))
      } yield e.head).transactionally)
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

  final def createManyQuery(entities: Seq[DbModel]): FixedSqlAction[Seq[DbModel], NoStream, Write] = (tableQuery returning tableQuery) ++= entities

  protected final def createOrUpdate(entity: DbModel): Future[Option[DbModel]] = db.run((tableQuery returning tableQuery).insertOrUpdate(entity))

  protected final def filterBy(tableFilter: List[TableFilter[T]], validOnly: Boolean = true, sinceLastModified: Option[String] = None): Query[T, DbModel, Seq] = {
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

  final def getMany(ids: List[UUID], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Seq[LwmModel]] = {
    val query = filterBy(validOnly, sinceLastModified, tableQuery.filter(_.id.inSet(ids)))

    if (atomic) toAtomic(query) else toUniqueEntity(query)
  }

  // TODO use this function instead of get(tableFilter) for a single entity
  final def getById(id: String, atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Option[LwmModel]] = {
    getMany(List(UUID.fromString(id)), atomic, validOnly, sinceLastModified).map(_.headOption)
  }

  final def delete(entity: DbModel): Future[Option[Timestamp]] = delete(entity.id)

  final def delete(id: UUID): Future[Option[Timestamp]] = db.run(delete0(id))

  final def deleteManyEntities(ids: List[DbModel]): Future[List[Option[Timestamp]]] = deleteMany(ids.map(_.id))

  final def deleteMany(ids: List[UUID]): Future[List[Option[Timestamp]]] = {
    val query = ids.map(delete0)

    db.run(DBIO.sequence(query))
  }

  private final def delete0(id: UUID) = {
    val now = DateTime.now.timestamp
    val found = tableQuery.filter(_.id === id)
    val query = found.result.head.flatMap { existing =>
      (for {
        u1 <- found.update(existing)
        u2 <- found.map(f => (f.lastModified, f.invalidated)).update((now, Some(now)))
      } yield u1 + u2).transactionally.map { rowsAffected =>
        if (rowsAffected >= 2) Some(existing) else None
      }
    }

    databaseExpander.fold(query) { expander =>
      (for {
        q <- query if q.isDefined
        e <- expander.expandDeleteOf(q.get)
      } yield e).transactionally
    }.map(_.map(_ => now))
  }

  final def update(entity: DbModel): Future[Option[DbModel]] = db.run(update0(entity))

  final def updateMany(entities: List[DbModel]): Future[List[Option[DbModel]]] = {
    val query = entities.map(update0)

    db.run(DBIO.sequence(query))
  }

  private final def update0(entity: DbModel) = {
    val found = tableQuery.filter(_.id === entity.id)
    val query = found.result.head.flatMap { existing =>
      if (shouldUpdate(existing, entity))
        (for {
          u1 <- found.update(entity)
          u2 <- found.map(_.lastModified).update(DateTime.now.timestamp)
        } yield u1 + u2).transactionally.map { rowsAffected =>
          if (rowsAffected >= 2) Some(entity) else None
        }
      else
        DBIO.failed(ModelAlreadyExists(existing))
    }

    databaseExpander.fold(query) { expander =>
      (for {
        q <- query if q.isDefined
        e <- expander.expandUpdateOf(q.get)
      } yield e).transactionally
    }
  }

  def createSchema: Future[Unit] = db.run(tableQuery.schema.create)

  def dropSchema: Future[Unit] = db.run(tableQuery.schema.create)
}
