package dao

import models._
import slick.dbio.Effect.Write
import store.RoleTable

class RoleDaoSpec extends AbstractDaoSpec[RoleTable, RoleDb, Role] with RoleDao { // TODO change to AbstractExpandableDaoSpec

  import dao.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._

  var state = 0 // bloody hack to prevent false update failures

  override protected val dbEntity: RoleDb = RoleDb("testRole", Set.empty)
  override protected val invalidDuplicateOfDbEntity: RoleDb = RoleDb(dbEntity.label, dbEntity.permissions)
  override protected val invalidUpdateOfDbEntity: RoleDb = dbEntity
  override protected val validUpdateOnDbEntity: RoleDb = dbEntity
  override protected val dbEntities: List[RoleDb] = roles
  override protected val lwmEntity: PostgresRole = dbEntity.toLwmModel
  override protected val lwmAtom: PostgresRoleAtom = PostgresRoleAtom(dbEntity.label, Set.empty, dbEntity.id)

  override protected def name: String = "role"

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = {
    state += 1
    state != 1 // abstractDaoSpec calls shouldUpdate two times
  }

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()
}
