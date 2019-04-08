package dao

import dao.helper.TableFilterable
import database.{BlacklistDb, BlacklistTable}
import javax.inject.Inject
import models.Blacklist
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

import scala.concurrent.{ExecutionContext, Future}

//case class BlacklistDateFilter(value: String) extends TableFilter[BlacklistTable] {
//  override def predicate = _.onDate(value)
//}
//
//case class BlacklistStartFilter(value: String) extends TableFilter[BlacklistTable] {
//  override def predicate = _.onStart(value)
//}
//
//case class BlacklistEndFilter(value: String) extends TableFilter[BlacklistTable] {
//  override def predicate = _.onEnd(value)
//}
//
//case class BlacklistSinceFilter(value: String) extends TableFilter[BlacklistTable] {
//  override def predicate = _.since(value)
//}
//
//case class BlacklistUntilFilter(value: String) extends TableFilter[BlacklistTable] {
//  override def predicate = _.until(value)
//}

object BlacklistDao extends TableFilterable[BlacklistTable] { // TODO date filter
//  def globalFilter(global: Boolean): TableFilterPredicate = _.global === global
//
//  def labelFilter(label: String): TableFilterPredicate = TableFilterable.labelFilter(label)
//
//  def dateFilter(date: LocalDate): TableFilterPredicate =
}

trait BlacklistDao extends AbstractDao[BlacklistTable, BlacklistDb, Blacklist] {

  override val tableQuery = TableQuery[BlacklistTable]

  override protected def toAtomic(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[Blacklist]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[Blacklist]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: BlacklistDb): Query[BlacklistTable, BlacklistDb, Seq] = {
    filterBy(List(
//      BlacklistLabelFilter(entity.label),
//      BlacklistDateFilter(entity.date.stringMillis),
//      BlacklistStartFilter(entity.start.stringMillis),
//      BlacklistEndFilter(entity.end.stringMillis),
//      BlacklistGlobalFilter(entity.global.toString)
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
