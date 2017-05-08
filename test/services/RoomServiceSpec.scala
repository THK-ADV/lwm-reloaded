package services

import models.{PostgresRoom, RoomDb}
import slick.driver.PostgresDriver.api._
import slick.dbio.Effect.Write
import store.RoomTable

final class RoomServiceSpec extends AbstractDaoSpec[RoomTable, RoomDb, PostgresRoom] with RoomService {

  import services.AbstractDaoSpec._

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "room"

  override protected val dbEntity: RoomDb = RoomDb("label", "description")

  override protected val invalidDuplicateOfDbEntity: RoomDb = RoomDb(dbEntity.label, "description2")

  override protected val invalidUpdateOfDbEntity: RoomDb = dbEntity.copy("new label", dbEntity.description)

  override protected val validUpdateOnDbEntity: RoomDb = dbEntity.copy(dbEntity.label, "new description")

  override protected val dbEntities: List[RoomDb] = rooms

  override protected val lwmEntity: PostgresRoom = dbEntity.toRoom

  override protected val lwmAtom: PostgresRoom = lwmEntity
}
