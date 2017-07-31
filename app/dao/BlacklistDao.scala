package dao

import models.LwmDateTime._
import models.{BlacklistDb, PostgresBlacklist}
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{BlacklistTable, TableFilter}

import scala.concurrent.Future

case class BlacklistGlobalFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.global === value.toBoolean
}

case class BlacklistLabelFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class BlacklistDateFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.date === value.sqlDate
}

case class BlacklistStartFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.start === value.sqlTime
}

case class BlacklistEndFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.end === value.sqlTime
}

case class BlacklistSinceFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.date >= value.sqlDate
}

case class BlacklistUntilFilter(value: String) extends TableFilter[BlacklistTable] {
  override def predicate = _.date <= value.sqlDate
}

trait BlacklistDao extends AbstractDao[BlacklistTable, BlacklistDb, PostgresBlacklist] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery = TableQuery[BlacklistTable]

  override protected def toAtomic(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[PostgresBlacklist]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[BlacklistTable, BlacklistDb, Seq]): Future[Seq[PostgresBlacklist]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }

  override protected def existsQuery(entity: BlacklistDb): Query[BlacklistTable, BlacklistDb, Seq] = {
    filterBy(List(
      BlacklistDateFilter(entity.date.string),
      BlacklistStartFilter(entity.start.string),
      BlacklistEndFilter(entity.end.string),
      BlacklistGlobalFilter(entity.global.toString)
    ))
  }

  override protected def shouldUpdate(existing: BlacklistDb, toUpdate: BlacklistDb): Boolean = {
    existing.label != toUpdate.label &&
      (existing.date.localDate.isEqual(toUpdate.date.localDate) &&
        existing.start.localTime.isEqual(toUpdate.start.localTime) &&
        existing.end.localTime.isEqual(toUpdate.end.localTime) &&
        existing.global == toUpdate.global)
  }
}

final class BlacklistDaoImpl(val db: PostgresDriver.backend.Database) extends BlacklistDao
