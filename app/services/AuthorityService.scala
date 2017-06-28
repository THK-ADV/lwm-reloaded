package services

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep
import store._

import scala.concurrent.Future

case class AuthorityUserFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: (AuthorityTable) => Rep[Boolean] = _.user === UUID.fromString(value)
}

case class AuthorityCourseFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: (AuthorityTable) => Rep[Boolean] = _.course.map(_ === UUID.fromString(value)).getOrElse(false)
}

case class AuthorityRoleFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: (AuthorityTable) => Rep[Boolean] = _.role === UUID.fromString(value)
}

trait AuthorityService extends AbstractDao[AuthorityTable, AuthorityDb, Authority] {
  import scala.concurrent.ExecutionContext.Implicits.global

  protected def roleService: RoleService2

  override val tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  override protected def shouldUpdate(existing: AuthorityDb, toUpdate: AuthorityDb): Boolean = false

  override protected def existsQuery(entity: AuthorityDb): Query[AuthorityTable, AuthorityDb, Seq] = {
    filterBy(List(AuthorityUserFilter(entity.user.toString), AuthorityRoleFilter(entity.role.toString))).filter(_.course === entity.course)
  }

  // TODO refactor based on AssignmentPlanService.toAtomic
  override protected def toAtomic(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[Authority]] = {
    val joinedQuery = for { // TODO wait for https://github.com/slick/slick/issues/179
      ((a, c), l) <- query.joinLeft(TableQuery[CourseTable]).on(_.course === _.id).
        joinLeft(TableQuery[UserTable]).on(_._2.map(_.lecturer) === _.id)
      u <- a.userFk
      r <- a.roleFk
    } yield (a, u, r, c, l)

    db.run(joinedQuery.result.map(_.foldLeft(List.empty[PostgresAuthorityAtom]) {
      case (list, (a, u, r, Some(c), Some(l))) =>
        val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, l.toLwmModel, c.semesterIndex, c.id)
        val atom = PostgresAuthorityAtom(u.toLwmModel, r.toLwmModel, Some(courseAtom), a.id)

        list.+:(atom)
      case (list, (a, u, r, None, None)) =>
        list.+:(PostgresAuthorityAtom(u.toLwmModel, r.toLwmModel, None, a.id))
      case (list, _) => list // this should never happen
    }))
  }

  override protected def toUniqueEntity(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[Authority]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }

  // TODO maybe we can do this with Expander
  def createWith(dbUser: DbUser): Future[PostgresAuthorityAtom] = {
    for {
      role <- roleService.byUserStatus(dbUser.status)
      created <- role.fold[Future[AuthorityDb]](Future.failed(new Throwable(s"No appropriate Role found while resolving user $dbUser"))) { role =>
        val authority = AuthorityDb(dbUser.id, role.id)
        create(authority)
      }
    } yield PostgresAuthorityAtom(dbUser.toLwmModel, role.map(Role.toRole).get, None, created.id)
  }
}

final class AuthorityServiceImpl(val db: PostgresDriver.backend.Database, val roleService: RoleService2) extends AuthorityService