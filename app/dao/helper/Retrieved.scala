package dao.helper

import java.sql.Timestamp
import java.util.UUID

import database.{TableFilter, UniqueTable}
import models.{UniqueDbEntity, UniqueEntity}
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep

import scala.collection.Traversable
import scala.concurrent.Future

trait Retrieved[T <: Table[DbModel] with UniqueTable, DbModel <: UniqueDbEntity, LwmModel <: UniqueEntity] {
  self: Core with Accessible[T, DbModel] =>

  protected def toAtomic(query: Query[T, DbModel, Seq]): Future[Traversable[LwmModel]]

  protected def toUniqueEntity(query: Query[T, DbModel, Seq]): Future[Traversable[LwmModel]]

  final def filterBy(tableFilter: List[TableFilter[T]], validOnly: Boolean = true, sinceLastModified: Option[String] = None): Query[T, DbModel, Seq] = {
    val query = tableFilter match {
      case h :: t =>
        t.foldLeft(tableQuery.filter(h.predicate)) { (query, nextFilter) =>
          query.filter(nextFilter.predicate)
        }
      case _ => tableQuery
    }

    query.filterBy(validOnly, sinceLastModified)
  }

  final def get(tableFilter: List[TableFilter[T]] = List.empty, atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Traversable[LwmModel]] =
    filterBy(tableFilter, validOnly, sinceLastModified)
      .retrieve(atomic)

  final def getByQuery(query: Query[T, DbModel, Seq], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Traversable[LwmModel]] =
    query.filterBy(validOnly, sinceLastModified)
      .retrieve(atomic)

  final def getMany(ids: List[UUID], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Traversable[LwmModel]] =
    tableQuery
      .filter(_.id.inSet(ids))
      .filterBy(validOnly, sinceLastModified)
      .retrieve(atomic)

  final def getManyQuery(ids: List[UUID], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None) =
    tableQuery
      .filter(_.id.inSet(ids))
      .filterBy(validOnly, sinceLastModified)

  final def getSingle(id: UUID, atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Option[LwmModel]] =
    tableQuery
      .filter(_.id === id)
      .take(1)
      .filterBy(validOnly, sinceLastModified)
      .retrieve(atomic)
      .map(_.headOption)

  final def getSingleWhere(where: T => Rep[Boolean], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Option[LwmModel]] =
    tableQuery
      .filter(where)
      .take(1)
      .filterBy(validOnly, sinceLastModified)
      .retrieve(atomic)
      .map(_.headOption)

  final def filter(where: T => Rep[Boolean], atomic: Boolean = true, validOnly: Boolean = true, sinceLastModified: Option[String] = None): Future[Traversable[LwmModel]] =
    tableQuery
      .filter(where)
      .filterBy(validOnly, sinceLastModified)
      .retrieve(atomic)

  final def filterValidOnly(where: T => Rep[Boolean]): Query[T, DbModel, Seq] = filterValidOnly(tableQuery.filter(where))

  final def filterValidOnly(query: Query[T, DbModel, Seq]): Query[T, DbModel, Seq] = query.filterBy(validOnly = true, None)

  protected implicit class QueryOps(val query: Query[T, DbModel, Seq]) {
    def retrieve(atomic: Boolean): Future[Traversable[LwmModel]] = if (atomic) toAtomic(query) else toUniqueEntity(query)

    def filterBy(validOnly: Boolean, sinceLastModified: Option[String]): Query[T, DbModel, Seq] = query.filterLastModified(sinceLastModified).filterValidOnly(validOnly)

    protected def filterValidOnly(validOnly: Boolean): Query[T, DbModel, Seq] = if (validOnly) query.filter(_.isValid) else query

    protected def filterLastModified(sinceLastModified: Option[String]): Query[T, DbModel, Seq] = {
      sinceLastModified.fold(query)(t => query.filter(_.lastModifiedSince(new Timestamp(t.toLong))))
    }
  }

}
