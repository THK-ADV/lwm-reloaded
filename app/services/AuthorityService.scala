package services

import java.util.UUID

import models._
import store.{AuthorityTable, CourseTable, PostgresDatabase, UserTable}

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._

trait AuthorityService extends AbstractDao[AuthorityTable, AuthorityDb, Authority] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  protected def roleService: RoleService2

  override val tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  override protected def toAtomic(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[Authority]] = {
    val joinedQuery = for {
      ((a, c), l) <- query.joinLeft(TableQuery[CourseTable]).on(_.course === _.id).
        joinLeft(TableQuery[UserTable]).on(_._2.map(_.lecturer) === _.id)
      u <- a.userFk
      r <- a.roleFk
    } yield (a, u, r, c, l)

    db.run(joinedQuery.result.map(_.foldLeft(List.empty[PostgresAuthorityAtom]) {
      case (list, (a, u, r, Some(c), Some(l))) =>
        val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, l.toUser, c.semesterIndex, c.id)
        val atom = PostgresAuthorityAtom(u.toUser, r.toRole, Some(courseAtom), a.id)

        list.+:(atom)
      case (list, (a, u, r, None, None)) =>
        list.+:(PostgresAuthorityAtom(u.toUser, r.toRole, None, a.id))
      case (list, _) => list // this should never happen
    }))
  }

  override protected def toUniqueEntity(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[Authority]] = {
    db.run(query.result.map(_.map(_.toAuthority)))
  }

  def createWith(dbUser: DbUser): Future[PostgresAuthorityAtom] = {
    for {
      role <- roleService.byUserStatus(dbUser.status)
      created <- role.fold[Future[AuthorityDb]](Future.failed(new Throwable(s"No appropriate Role found while resolving user $dbUser"))) { role =>
        val authority = AuthorityDb(dbUser.id, role.id, None, None, UUID.randomUUID)
        create(authority)
      }
    } yield PostgresAuthorityAtom(dbUser.toUser, role.map(Role.toRole).get, None, created.id)
  }
}

object AuthorityService extends AuthorityService with PostgresDatabase {
  override protected def roleService: RoleService2 = RoleService2
}
