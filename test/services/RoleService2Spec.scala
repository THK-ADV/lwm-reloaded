package services

import base.PostgresDbSpec
import models.{Role, RoleDb}
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.RoleTable

class RoleService2Spec extends AbstractDaoSpec[RoleTable, RoleDb, Role] with RoleService2 {
  override protected val rolePermissionService: RolePermissionService = new RolePermissionServiceSpec()

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = ???

  override protected def name: String = "role"

  override protected def entity: RoleDb = ???

  override protected def invalidDuplicateOfEntity: RoleDb = ???

  override protected def invalidUpdateOfEntity: RoleDb = ???

  override protected def validUpdateOnEntity: RoleDb = ???

  override protected def entities: List[RoleDb] = ???
}
