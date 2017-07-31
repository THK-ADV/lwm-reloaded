package dao

import models.{PostgresRoom, RoomDb}
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.RoomTable

final class RoomDaoSpec extends AbstractDaoSpec[RoomTable, RoomDb, PostgresRoom] with RoomDao {

  import dao.AbstractDaoSpec._

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "room"

  override protected val dbEntity: RoomDb = RoomDb("label", "description", 0)

  override protected val invalidDuplicateOfDbEntity: RoomDb = RoomDb(dbEntity.label, "description2", 100)

  override protected val invalidUpdateOfDbEntity: RoomDb = dbEntity.copy("new label", dbEntity.description)

  override protected val validUpdateOnDbEntity: RoomDb = dbEntity.copy(dbEntity.label, "new description")

  override protected val dbEntities: List[RoomDb] = rooms

  override protected val lwmEntity: PostgresRoom = dbEntity.toLwmModel

  override protected val lwmAtom: PostgresRoom = lwmEntity
}
