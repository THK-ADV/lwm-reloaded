package dao

import java.util.UUID

import javax.inject.Inject
import models.Room
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import store.{RoomDb, RoomTable, TableFilter}

import scala.concurrent.Future

case class RoomIdFilter(value: String) extends TableFilter[RoomTable] {
  override def predicate = _.id === UUID.fromString(value)
}

case class RoomLabelFilter(value: String) extends TableFilter[RoomTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

trait RoomDao extends AbstractDao[RoomTable, RoomDb, Room] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[RoomTable] = TableQuery[RoomTable]

  override protected def existsQuery(entity: RoomDb): Query[RoomTable, RoomDb, Seq] = {
    filterBy(List(RoomLabelFilter(entity.label)))
  }

  override protected def shouldUpdate(existing: RoomDb, toUpdate: RoomDb): Boolean = {
    (existing.description != toUpdate.description ||
      existing.capacity != toUpdate.capacity) &&
      existing.label == toUpdate.label
  }

  override protected def toAtomic(query: Query[RoomTable, RoomDb, Seq]): Future[Seq[Room]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[RoomTable, RoomDb, Seq]): Future[Seq[Room]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class RoomDaoImpl @Inject()(val db: PostgresProfile.backend.Database) extends RoomDao