package dao

import database.{RoomDb, RoomTable}
import javax.inject.Inject
import models.Room
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

trait RoomDao extends AbstractDao[RoomTable, RoomDb, Room] {

  import dao.helper.TableFilter.labelFilterEquals

  override val tableQuery: TableQuery[RoomTable] = TableQuery[RoomTable]

  override protected def existsQuery(entity: RoomDb): Query[RoomTable, RoomDb, Seq] = {
    filterBy(List(labelFilterEquals(entity.label)))
  }

  override protected def shouldUpdate(existing: RoomDb, toUpdate: RoomDb): Boolean = {
    existing.label == toUpdate.label
  }

  override protected def toAtomic(query: Query[RoomTable, RoomDb, Seq]): Future[Seq[Room]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[RoomTable, RoomDb, Seq]): Future[Seq[Room]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class RoomDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends RoomDao