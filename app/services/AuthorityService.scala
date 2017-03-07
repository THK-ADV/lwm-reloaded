package services

import models.{DbUser, PostgresAuthority, PostgresAuthorityAtom}
import store.AuthorityTable

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._

trait AuthorityService extends AbstractDao[AuthorityTable, PostgresAuthority, PostgresAuthority] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  override protected def toAtomic(query: Query[AuthorityTable, PostgresAuthority, Seq]): Future[Seq[PostgresAuthority]] = ???

  override protected def toUniqueEntity(query: Query[AuthorityTable, PostgresAuthority, Seq]): Future[Seq[PostgresAuthority]] = ???

  def createWith(user: DbUser): Future[PostgresAuthorityAtom] = {
    for {
      role <- RoleService2.byUserStatus(user.status)
      created <- role.fold[Future[PostgresAuthority]](Future.failed(new Throwable(s"No appropriate Role found while resolving user $user"))) { role =>
        val authority = PostgresAuthority(user.id, role.id)
        create(authority)
      }
    } yield PostgresAuthorityAtom(user, role.get, None, created.id)
  }
}

object AuthorityService extends AuthorityService
