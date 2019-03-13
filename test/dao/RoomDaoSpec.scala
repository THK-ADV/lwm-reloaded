package dao

import models.Room
import slick.dbio.Effect.Write
import slick.jdbc.PostgresProfile.api._
import database.{RoomDb, RoomTable}
import play.api.inject.guice.GuiceableModule

final class RoomDaoSpec extends AbstractDaoSpec[RoomTable, RoomDb, Room] {

  import AbstractDaoSpec._

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "room"

  override protected val dbEntity: RoomDb = RoomDb("label", "description", 0)

  override protected val invalidDuplicateOfDbEntity: RoomDb = RoomDb(dbEntity.label, "description2", 100)

  override protected val invalidUpdateOfDbEntity: RoomDb = dbEntity.copy("new label", dbEntity.description)

  override protected val validUpdateOnDbEntity: RoomDb = dbEntity.copy(dbEntity.label, "new description")

  override protected val dbEntities: List[RoomDb] = rooms

  override protected val lwmAtom: Room = dbEntity.toUniqueEntity

  override protected val dao: AbstractDao[RoomTable, RoomDb, Room] = app.injector.instanceOf(classOf[RoomDao])

  override protected def bindings: Seq[GuiceableModule] = Seq.empty
}
