package services

import base.PostgresDbSpec
import models.RolePermission
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.RolePermissionTable

class RolePermissionServiceSpec extends AbstractDaoSpec[RolePermissionTable, RolePermission, RolePermission] with RolePermissionService {
  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = ???

  override protected def name: String = "rolePermission"

  override protected def entity: RolePermission = ???

  override protected def invalidDuplicateOfEntity: RolePermission = ???

  override protected def invalidUpdateOfEntity: RolePermission = ???

  override protected def validUpdateOnEntity: RolePermission = ???

  override protected def entities: List[RolePermission] = ???
}
