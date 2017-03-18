package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write

object RolePermissionServiceSpec extends PostgresDbSpec with RolePermissionService {
  override protected def customFill: _root_.slick.driver.PostgresDriver.api.DBIOAction[Unit, _root_.slick.driver.PostgresDriver.api.NoStream, Write] = ???
}
