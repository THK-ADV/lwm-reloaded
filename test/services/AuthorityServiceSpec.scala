package services

import base.PostgresDbSpec
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._

class AuthorityServiceSpec extends PostgresDbSpec with AuthorityService {
  override protected def roleService: RoleService2 = RoleService2Spec

  override protected def customFill: DBIOAction[Unit, NoStream, Write] = ???
}
