package services

import java.util.UUID

import models._
import store.AuthorityTable

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._

trait AuthorityService extends AbstractDao[AuthorityTable, AuthorityDb, PostgresAuthority] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  override protected def toAtomic(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[PostgresAuthority]] = ???

  override protected def toUniqueEntity(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[PostgresAuthority]] = ???

  def createWith(dbUser: DbUser): Future[PostgresAuthorityAtom] = {
    for {
      role <- RoleService2.byUserStatus(dbUser.status)
      created <- role.fold[Future[AuthorityDb]](Future.failed(new Throwable(s"No appropriate Role found while resolving user $dbUser"))) { role =>
        val authority = AuthorityDb(dbUser.id, role.id, None, None, UUID.randomUUID)
        create(authority)
      }
    } yield PostgresAuthorityAtom(dbUser.toUser, role.map(Role.toRole).get, None, created.id)
  }
}

object AuthorityService extends AuthorityService
