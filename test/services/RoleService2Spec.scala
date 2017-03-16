package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write

object RoleService2Spec extends PostgresDbSpec with RoleService2 {
  override protected def rolePermissionService: RolePermissionService = RolePermissionServiceSpec

  override protected def fillDb: _root_.slick.driver.PostgresDriver.api.DBIOAction[Unit, _root_.slick.driver.PostgresDriver.api.NoStream, Write] = ???
}
