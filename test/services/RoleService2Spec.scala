package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._

object RoleService2Spec extends PostgresDbSpec with RoleService2 {
  override protected def rolePermissionService: RolePermissionService = RolePermissionServiceSpec

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = ???
}
