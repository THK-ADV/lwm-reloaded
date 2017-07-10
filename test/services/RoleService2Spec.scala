package services

import models._
import slick.dbio.Effect.Write
import store.{PermissionTable, RoleTable}

/**
  * Created by florian on 7/10/17.
  */
class RoleService2Spec extends AbstractDaoSpec[RoleTable, RoleDb, Role] with RoleService2{

  import services.AbstractDaoSpec._
  import slick.driver.PostgresDriver.api._

  override protected def shouldUpdate(existing: RoleDb, toUpdate: RoleDb): Boolean = false

  override protected def name: String = "role"

  override protected val dbEntity: RoleDb = RoleDb("testRole", Set.empty)

  override protected val invalidDuplicateOfDbEntity: RoleDb = RoleDb(dbEntity.label, dbEntity.permissions)

  override protected val invalidUpdateOfDbEntity: RoleDb = dbEntity

  override protected val validUpdateOnDbEntity: RoleDb = dbEntity

  override protected val dbEntities: List[RoleDb] = roles

  override protected val lwmEntity: PostgresRole = dbEntity.toRole

  override protected val lwmAtom: PostgresRoleAtom  = PostgresRoleAtom(dbEntity.label, Set.empty, dbEntity.id)

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()
}
