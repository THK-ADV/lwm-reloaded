package services

import models.UniqueEntity
import slick.driver.PostgresDriver.api._
import store.{PostgresDatabase, TableFilter, UniqueTable}

import scala.concurrent.Future

case class ModelAlreadyExists[A](value: A) extends Throwable {
  override def getMessage = s"model already exists $value"
}

// TODO maybe we can get rid of DbModel
trait AbstractDao[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  def tableQuery: TableQuery[T]

  protected def toAtomic(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def toUniqueEntity(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def setInvalidated(entity: DbModel): DbModel

  protected def existsQuery(entity: DbModel): Query[T, DbModel, Seq]

  protected def shouldUpdate(existing: DbModel, toUpdate: DbModel): Boolean

  final def create(entity: DbModel): Future[DbModel] = {
    val query = existsQuery(entity).result.flatMap { exists =>
      if (exists.nonEmpty)
        DBIO.failed(ModelAlreadyExists(exists))
      else
        (tableQuery returning tableQuery) += entity
    }

    db.run(query)
  }

  final def createMany(entities: List[DbModel]): Future[Seq[DbModel]] = db.run((tableQuery returning tableQuery) ++= entities)

  protected final def createOrUpdate(entity: DbModel): Future[Option[DbModel]] = db.run((tableQuery returning tableQuery).insertOrUpdate(entity))

  protected final def filterBy(tableFilter: List[TableFilter[T]], validOnly: Boolean = true): Query[T, DbModel, Seq] = {
    val query = tableFilter match {
      case h :: t =>
        t.foldLeft(tableQuery.filter(h.predicate)) { (query, nextFilter) =>
          query.filter(nextFilter.predicate)
        }
      case _ => tableQuery
    }

    if (validOnly) query.filter(_.isValid) else query
  }

  final def get(tableFilter: List[TableFilter[T]] = List.empty, atomic: Boolean = true, validOnly: Boolean = true): Future[Seq[LwmModel]] = {
    val query = filterBy(tableFilter, validOnly)

    if (atomic) toAtomic(query) else toUniqueEntity(query)
  }

  final def delete(entity: DbModel): Future[Option[DbModel]] = {
    val invalidated = setInvalidated(entity)
    val query = tableQuery.filter(_.id === invalidated.id).update(invalidated).map {
      case 1 => Some(entity)
      case _ => None
    }

    db.run(query)
  }

  final def update(entity: DbModel): Future[Option[DbModel]] = {
    val found = tableQuery.filter(_.id === entity.id)
    val query = found.result.head.flatMap { existing =>
      if (shouldUpdate(existing, entity))
        found.update(entity).map {
          case 1 => Some(entity)
          case _ => None
        }
      else
        DBIO.failed(ModelAlreadyExists(existing))
    }

    db.run(query)
  }

  final def createSchema = db.run(tableQuery.schema.create)

  final def dropSchema = db.run(tableQuery.schema.create)
}
