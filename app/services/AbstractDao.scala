package services

import models.UniqueEntity
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep
import store.{PostgresDatabase, TableFilter, UniqueTable}

import scala.concurrent.Future

case object ModelAlreadyExists extends Throwable {
  override def getMessage = "model already exists"
}

// TODO maybe we can get rid of DbModel
trait AbstractDao[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueEntity, LwmModel <: UniqueEntity] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  def tableQuery: TableQuery[T]

  protected def toAtomic(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def toUniqueEntity(query: Query[T, DbModel, Seq]): Future[Seq[LwmModel]]

  protected def setInvalidated(entity: DbModel): DbModel

//  protected def shouldUpdate(existing: DbModel, toCreate: DbModel): Boolean

  protected def existsQuery(entity: DbModel): Query[T, DbModel, Seq]

  final def create(entity: DbModel): Future[DbModel] = {
    val query = existsQuery(entity).exists.result.flatMap { exists =>
      if (exists)
        DBIO.failed(ModelAlreadyExists)
      else
        (tableQuery returning tableQuery) += entity
    }

    db.run(query)
  }

  final def createMany(entities: List[DbModel]): Future[Seq[DbModel]] = db.run((tableQuery returning tableQuery) ++= entities)

  final def createOrUpdate(entity: DbModel): Future[Option[DbModel]] = db.run((tableQuery returning tableQuery).insertOrUpdate(entity))

  final def get(tableFilter: List[TableFilter[T]] = List.empty, atomic: Boolean = true, validOnly: Boolean = true): Future[Seq[LwmModel]] = {
    val query = tableFilter match {
      case h :: t =>
        t.foldLeft(tableQuery.filter(h.predicate)) { (query, nextFilter) =>
        query.filter(nextFilter.predicate)
      }
      case _ => tableQuery
    }
    
    val valid = if (validOnly) query.filter(_.isValid) else query

    if (atomic) toAtomic(valid) else toUniqueEntity(valid)
  }

  final def delete(entity: DbModel): Future[Option[DbModel]] = update0(setInvalidated(entity))

  final def update(entity: DbModel): Future[Option[DbModel]] = update0(entity)

  private final def update0(entity: DbModel): Future[Option[DbModel]] = db.run(
    tableQuery.filter(_.id === entity.id).update(entity).map {
      case 1 => Some(entity)
      case _ => None
    }
  )

  final def createSchema = db.run(tableQuery.schema.create)

  final def dropSchema = db.run(tableQuery.schema.create)
}
