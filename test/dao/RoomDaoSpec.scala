package dao

import java.util.UUID

import database.{RoomDb, RoomTable}
import models.Room
import play.api.inject.guice.GuiceableModule
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._

final class RoomDaoSpec extends AbstractDaoSpec[RoomTable, RoomDb, Room] {

  import AbstractDaoSpec._

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "room"

  override protected val dbEntity: RoomDb =
    RoomDb("label", "description", 0)

  override protected val invalidDuplicateOfDbEntity: RoomDb =
    dbEntity.copy(id = UUID.randomUUID)

  override protected val invalidUpdateOfDbEntity: RoomDb =
    dbEntity.copy(label = "updated")

  override protected val validUpdateOnDbEntity: RoomDb =
    dbEntity.copy(description = "new description", capacity = dbEntity.capacity + 1)

  override protected val dbEntities: List[RoomDb] = rooms

  override protected val lwmAtom: Room = dbEntity.toUniqueEntity

  override protected val dao: AbstractDao[RoomTable, RoomDb, Room] = app.injector.instanceOf(classOf[RoomDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
