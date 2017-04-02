package services

import java.util.UUID

import models.{PostgresRoom, RoomDb}
import org.joda.time.DateTime
import store.{PostgresDatabase, RoomTable, TableFilter}

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._

case class RoomIdFilter(value: String) extends TableFilter[RoomTable] {
  override def predicate = _.id === UUID.fromString(value)
}
case class RoomLabelFilter(value: String) extends TableFilter[RoomTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

trait RoomService extends AbstractDao[RoomTable, RoomDb, PostgresRoom] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[RoomTable] = TableQuery[RoomTable]

  override protected def existsQuery(entity: RoomDb): Query[RoomTable, RoomDb, Seq] = {
    filterBy(List(RoomLabelFilter(entity.label)))
  }

  override protected def shouldUpdate(existing: RoomDb, toUpdate: RoomDb): Boolean = {
    existing.description != toUpdate.description &&
    existing.label == toUpdate.label
  }

  override protected def setInvalidated(entity: RoomDb): RoomDb = {
    RoomDb(entity.label, entity.description, Some(DateTime.now), entity.id)
  }

  override protected def toAtomic(query: Query[RoomTable, RoomDb, Seq]): Future[Seq[PostgresRoom]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[RoomTable, RoomDb, Seq]): Future[Seq[PostgresRoom]] = {
    db.run(query.result.map(_.map(_.toRoom)))
  }
}

object RoomService extends RoomService with PostgresDatabase