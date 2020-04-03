package dao

import dao.helper.TableFilter
import database.{BlacklistDb, BlacklistTable}
import javax.inject.Inject
import models.Blacklist
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object BlacklistDao extends TableFilter[BlacklistTable] {
  def globalFilter(global: Boolean): TableFilterPredicate = _.global === global
}

trait BlacklistDao extends AbstractDao[BlacklistTable, BlacklistDb, Blacklist] {

  import TableFilter.{labelFilterEquals, onDateFilter}

  override val tableQuery = TableQuery[BlacklistTable]

  override protected def toAtomic(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[Blacklist]] =
    toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[Blacklist]] =
    db.run(query.result.map(_.map(_.toUniqueEntity)))

  override protected def existsQuery(entity: BlacklistDb): Query[BlacklistTable, BlacklistDb, Seq] =
    if (entity.global)
      filterValidOnly(b => b.global && onDateFilter(entity.date).apply(b))
    else
      filterValidOnly(b => b.global && onDateFilter(entity.date).apply(b) || onDateFilter(entity.date).apply(b) && labelFilterEquals(entity.label).apply(b))

  override protected def shouldUpdate(from: BlacklistDb, to: BlacklistDb): Boolean =
    if (to.global)
      from.global == to.global && from.date == to.date
    else
      from.global == to.global && (from.date == to.date || from.label == to.label)
}

final class BlacklistDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends BlacklistDao
