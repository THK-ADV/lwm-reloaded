package dao

import java.util.UUID

import javax.inject.{Inject, Singleton}
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
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
case class AuthorityRoleLabelFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: (AuthorityTable) => Rep[Boolean] = _.roleFk.map(_.label).filter(_ === value).exists
}

trait AuthorityDao extends AbstractDao[AuthorityTable, AuthorityDb, Authority] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  protected def roleService: RoleDao

  // TODO maybe we can do this with Expander
  def createWith(dbUser: DbUser): Future[PostgresAuthorityAtom] = {
    for {
      role <- roleService.byUserStatus(dbUser.status)
      created <- role.fold[Future[AuthorityDb]](Future.failed(new Throwable(s"No appropriate Role found while resolving user $dbUser"))) { role =>
        val authority = AuthorityDb(dbUser.id, role.id)
        create(authority)
      }
    } yield PostgresAuthorityAtom(dbUser.toLwmModel, role.map(_.toLwmModel).get, None, created.id)
  }

  def createByCourseQuery(course: CourseDb) = {
    (for {
      cm <- roleService.byRoleLabelQuery(Roles.CourseManagerLabel)
      rm <- roleService.byRoleLabelQuery(Roles.RightsManagerLabel)

      rma = AuthorityDb(course.lecturer, rm.head.id)
      cma = AuthorityDb(course.lecturer, cm.head.id, Some(course.id))

      hasRM <- filterBy(List(AuthorityUserFilter(course.lecturer.toString), AuthorityRoleFilter(rm.head.id.toString))).exists.result
      authoritiesToCreate = if (hasRM) Seq(cma) else Seq(cma, rma)

      c <- createManyQuery(authoritiesToCreate)
    } yield c).transactionally
  }

  def updateByCourseQuery(oldCourse: CourseDb, newCourse: CourseDb) = {
    DBIO.seq(
      deleteByCourseQuery(oldCourse),
      createByCourseQuery(newCourse)
    ).transactionally
  }

  def updateByCourse(oldCourse: CourseDb, newCourse: CourseDb) = {
    db.run(updateByCourseQuery(oldCourse, newCourse))
  }

  final def deleteByCourseQuery(course: CourseDb) = {
    for {
      deleted <- filterBy(List(AuthorityCourseFilter(course.id.toString), AuthorityUserFilter(course.lecturer.toString))).delete
      deletedAuthority <- deleteSingleRightsManagerQuery(course.lecturer)
    } yield deletedAuthority + deleted
  }

  final def deleteByCourse(course: CourseDb): Future[Int] = {
    db.run(deleteByCourseQuery(course))
  }

  def deleteSingleRightsManagerQuery(lecturer: UUID)= {
    for {
      hasCourse <- filterBy(List(AuthorityUserFilter(lecturer.toString))).filter(_.course.isDefined).exists.result
      rm <- roleService.tableQuery.filter(_.label === Roles.RightsManagerLabel).map(_.id).result.headOption if rm.isDefined
      deletedRM <- {
        if (hasCourse) {
          DBIO.successful(0)
        } else {
          filterBy(List(AuthorityUserFilter(lecturer.toString), AuthorityRoleFilter(rm.get.toString))).delete
        }
      }
    } yield deletedRM
  }

  override protected def shouldUpdate(existing: AuthorityDb, toUpdate: AuthorityDb): Boolean = {
    (existing.invalidated != toUpdate.invalidated ||
    existing.lastModified != toUpdate.lastModified) &&
      (existing.user == toUpdate.user && existing.course == toUpdate.course && existing.role == toUpdate.role)
  }

  def authoritiesFor(userId: UUID): Future[Seq[PostgresAuthority]] = { // TODO still needed? replaced with def authoritiesFor(systemId: String): Future[Seq[PostgresAuthority]]
    val authorities = for (q <- tableQuery if q.user === userId) yield q

    db.run(authorities.result.map(_.map(_.toLwmModel)))
  }

  def authoritiesFor(systemId: String): Future[Seq[PostgresAuthority]] = {
    val authorities = for {
      q <- tableQuery
      u <- q.userFk if u.systemId === systemId
    } yield q

    db.run(authorities.result.map(_.map(_.toLwmModel)))
  }

  def checkAuthority(check: (Option[UUID], List[Role]))(authorities: Seq[PostgresAuthority]): Future[Boolean] = check match {
    case (_, roles) if roles contains Role.God => Future.successful(false)
    case (optCourse, minRoles) =>
      def isAdmin(implicit roles: Seq[PostgresRole]) = roles
        .find(_.label == Role.Admin.label)
        .exists(admin => authorities.exists(_.role == admin.id))

      def hasPermission(implicit roles: Seq[PostgresRole]) = authorities
        .filter(_.course == optCourse)
        .flatMap(authority => roles.filter(_.id == authority.role))
        .exists(r => minRoles.exists(_.label == r.label))

      roleService.get().map { implicit roles =>
        isAdmin || hasPermission
      }
  }

  override protected def existsQuery(entity: AuthorityDb): Query[AuthorityTable, AuthorityDb, Seq] = {
    filterBy(List(
      AuthorityUserFilter(entity.user.toString),
      AuthorityRoleFilter(entity.role.toString))
    ).filter(_.course === entity.course)
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
}

@Singleton
final class AuthorityDaoImpl @Inject() (val db: PostgresProfile.backend.Database, val roleService: RoleDao) extends AuthorityDao