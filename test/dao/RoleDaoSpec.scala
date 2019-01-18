package dao

import models._
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver
import store.RoleTable

class RoleDaoSpec extends AbstractDaoSpec[RoleTable, RoleDb, PostgresRole] with RoleDao {

  import dao.AbstractDaoSpec._
  import slick.jdbc.PostgresProfile.api._

  override protected val dbEntity: RoleDb = RoleDb("testRole")
  override protected val invalidDuplicateOfDbEntity: RoleDb = dbEntity
  override protected val invalidUpdateOfDbEntity: RoleDb = dbEntity
  override protected val validUpdateOnDbEntity: RoleDb = dbEntity
  override protected val dbEntities: List[RoleDb] = roles
  override protected val lwmEntity: PostgresRole = dbEntity.toLwmModel
  override protected val lwmAtom: PostgresRole = lwmEntity
  var state = 0 // bloody hack to prevent false update failures

  override protected def name: String = "role"

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = {
    state += 1
    state != 1 // abstractDaoSpec calls shouldUpdate two times
  }

  override protected def dependencies: PostgresDriver.api.DBIOAction[Unit, PostgresDriver.api.NoStream, Write] = DBIO.seq()
}
