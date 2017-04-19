package services

import base.PostgresDbSpec
import models.{Authority, AuthorityDb}
import slick.dbio.Effect.Write
import slick.driver.PostgresDriver.api._
import store.AuthorityTable

class AuthorityServiceSpec extends AbstractDaoSpec[AuthorityTable, AuthorityDb, Authority] with AuthorityService {
  override protected val roleService: RoleService2 = new RoleService2Spec()

  override protected def dependencies: DBIOAction[Unit, NoStream, Write] = DBIO.seq()

  override protected def name: String = "authority"

  override protected def entity: AuthorityDb = ???

  override protected def invalidDuplicateOfEntity: AuthorityDb = ???

  override protected def invalidUpdateOfEntity: AuthorityDb = ???

  override protected def validUpdateOnEntity: AuthorityDb = ???

  override protected def entities: List[AuthorityDb] = ???
}
