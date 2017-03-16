package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write

class AuthorityServiceSpec extends PostgresDbSpec with AuthorityService {
  override protected def roleService: RoleService2 = RoleService2Spec

  override protected def fillDb: _root_.slick.driver.PostgresDriver.api.DBIOAction[Unit, _root_.slick.driver.PostgresDriver.api.NoStream, Write] = ???
}
