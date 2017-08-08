package dao

import models._
import slick.dbio.Effect.Write
import store.{PermissionTable, RoleTable}

class RoleDaoSpec extends AbstractExpandableDaoSpec[RoleTable, RoleDb, Role] with RoleDao {

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

  val permissions = (0 until 200).map { i =>
    PermissionDb(i.toString, i.toString)
  }.toList

  override protected val dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq(
    TableQuery[PermissionTable].forceInsertAll(permissions)
  )

  override protected val toAdd: List[RoleDb] = (0 until 20).map { i =>
    RoleDb(s"fake $i", takeSomeOf(permissions).map(_.id).toSet)
  }.toList

  override protected val numberOfUpdates: Int = 1

  override protected val numberOfDeletions: Int = 3

  override protected def update(toUpdate: List[RoleDb]): List[RoleDb] = toUpdate.map(r => r.copy(permissions = Set.empty))

  override protected def atom(dbModel: RoleDb): Role = PostgresRoleAtom(
    dbModel.label,
    permissions.filter(p => dbModel.permissions.contains(p.id)).map(_.toLwmModel).toSet,
    dbModel.id
  )

  override protected def expanderSpecs(dbModel: RoleDb, isDefined: Boolean): DBIOAction[Unit, NoStream, Effect.Read] = DBIO.seq(
    rolePermissionQuery.filter(_.role === dbModel.id).result.map { rolePerms =>
      rolePerms.size shouldBe (if (isDefined) dbModel.permissions.size else 0)
      rolePerms.map(_.permission).toSet shouldBe (if (isDefined) dbModel.permissions else Set.empty)
    }
  )
}
