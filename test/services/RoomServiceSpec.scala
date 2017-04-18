package services

import models.{PostgresRoom, RoomDb}
import slick.driver.PostgresDriver.api._
import slick.dbio.Effect.Write
import store.RoomTable

final class RoomServiceSpec extends AbstractDaoSpec[RoomTable, RoomDb, PostgresRoom, PostgresRoom] with RoomService {

  import services.AbstractDaoSpec._

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "room"

  override protected val entity: RoomDb = RoomDb("label", "description")

  override protected val invalidDuplicateOfEntity: RoomDb = RoomDb(entity.label, "description2")

  override protected val invalidUpdateOfEntity: RoomDb = entity.copy("new label", entity.description)

  override protected val validUpdateOnEntity: RoomDb = entity.copy(entity.label, "new description")

  override protected val entities: List[RoomDb] = rooms

  override protected val postgresEntity: PostgresRoom = entity.toRoom

  override protected val postgresAtom: PostgresRoom = postgresEntity
}
