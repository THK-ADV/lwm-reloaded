package dao

import dao.helper.TableFilter
import database.{BlacklistDb, BlacklistTable}
import javax.inject.Inject
import models.Blacklist
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

object BlacklistDao extends TableFilter[BlacklistTable] {
  def globalFilter(global: Boolean): TableFilterPredicate = _.global === global
}

trait BlacklistDao extends AbstractDao[BlacklistTable, BlacklistDb, Blacklist] {

  import BlacklistDao.globalFilter
  import TableFilter.{labelFilterEquals, onDateFilter, onEndFilter, onStartFilter}

  override val tableQuery = TableQuery[BlacklistTable]

  override protected def toAtomic(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[Blacklist]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[Blacklist]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: BlacklistDb): Query[BlacklistTable, BlacklistDb, Seq] = {
    filterBy(List(
      labelFilterEquals(entity.label),
      onDateFilter(entity.date),
      onStartFilter(entity.start),
      onEndFilter(entity.end),
      globalFilter(entity.global)
    ))
  }

  override protected def shouldUpdate(existing: BlacklistDb, toUpdate: BlacklistDb): Boolean = {
    existing.label != toUpdate.label &&
      (existing.date.localDate == toUpdate.date.localDate &&
        existing.start.localTime == toUpdate.start.localTime &&
        existing.end.localTime == toUpdate.end.localTime &&
        existing.global == toUpdate.global)
  }
}

final class BlacklistDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends BlacklistDao
