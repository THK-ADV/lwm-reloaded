package dao

import java.util.UUID

import database._
import javax.inject.Inject
import models._
import slick.dbio.Effect
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep
import slick.sql.FixedSqlAction

import scala.concurrent.{ExecutionContext, Future}

case class AuthorityUserFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: AuthorityTable => Rep[Boolean] = _.user === UUID.fromString(value)
}

case class AuthorityCourseFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: AuthorityTable => Rep[Boolean] = _.course.map(_ === UUID.fromString(value)).getOrElse(false)
}

case class AuthorityRoleFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: AuthorityTable => Rep[Boolean] = _.role === UUID.fromString(value)
}

case class AuthorityRoleLabelFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: AuthorityTable => Rep[Boolean] = _.roleFk.filter(_.label === value).exists
}

case class AuthoritySystemIdFilter(value: String) extends TableFilter[AuthorityTable] {
  override def predicate: AuthorityTable => Rep[Boolean] = _.userFk.filter(_.systemId === value).exists // TODO FK comparision should be done this way
}

trait AuthorityDao extends AbstractDao[AuthorityTable, AuthorityDb, AuthorityLike] {

  override val tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  protected def roleDao: RoleDao

  def studentRole: LWMRole = Role.StudentRole

  def employeeRole: LWMRole = Role.EmployeeRole

  def courseManagerRole: LWMRole = Role.CourseManager

  def rightsManagerRole: LWMRole = Role.RightsManager

  private def hasAuthority(user: UUID, role: UUID): FixedSqlAction[Boolean, NoStream, Effect.Read] = {
    filterValidOnly(a => a.user === user && a.role === role).exists.result
  }

  def isCourseManager(user: UUID): FixedSqlAction[Boolean, NoStream, Effect.Read] = {
    val query = for {
      q <- tableQuery if q.user === user && q.course.isDefined
      r <- q.roleFk if r.label === courseManagerRole.label
    } yield q

    filterValidOnly(query).exists.result
  }

  def deleteCourseManagerQuery(course: CourseDb): FixedSqlAction[Int, NoStream, Effect.Write] = {
    filterValidOnly { a =>
      a.user === course.lecturer &&
        a.course.map(_ === course.id).getOrElse(false) &&
        a.roleFk.filter(_.label === courseManagerRole.label).exists
    }.delete
  }

  def deleteRightsManagerQuery(user: UUID): FixedSqlAction[Int, NoStream, Effect.Write] = {
    filterValidOnly { a =>
      a.user === user &&
        a.roleFk.filter(_.label === rightsManagerRole.label).exists
    }.delete
  }

  def createBasicAuthorityFor(user: UserDb): DBIOAction[AuthorityDb, NoStream, Effect.Read with Effect.Read with Effect.Write with Effect.Transactional] = {
    for {
      baseRole <- roleDao.byUserStatusQuery(user.status) if baseRole.isDefined
      baseAuth = AuthorityDb(user.id, baseRole.get.id)
      created <- createQuery(baseAuth)
    } yield created
  }

  def deleteAuthorityIfNotBasic(id: UUID): Future[AuthorityDb] = {
    val query = filterValidOnly {
      for {
        q <- tableQuery if q.id === id
        r <- q.roleFk if r.label =!= studentRole.label && r.label =!= employeeRole.label
      } yield q
    }

    val action = for {
      nonBasic <- query.exists.result
      d <- if (nonBasic)
        deleteQuery(id)
      else
        DBIO.failed(new Throwable(s"The user associated with $id have to remain with at least one basic role, namely ${studentRole.label} or ${employeeRole.label}"))
    } yield d

    db run action
  }

  def createAssociatedAuthorities(course: CourseDb) = {
    (for {
      courseManager <- roleDao.byRoleLabelQuery(courseManagerRole.label) if courseManager.isDefined
      rightsManager <- roleDao.byRoleLabelQuery(rightsManagerRole.label) if rightsManager.isDefined

      courseManagerAuth = AuthorityDb(course.lecturer, courseManager.head.id, Some(course.id))
      rightsManagerAuth = AuthorityDb(course.lecturer, rightsManager.head.id)

      isRightsManagerAlready <- hasAuthority(course.lecturer, rightsManager.head.id)
      toCreate = if (isRightsManagerAlready) Seq(courseManagerAuth) else Seq(courseManagerAuth, rightsManagerAuth)
      created <- createManyQuery(toCreate)
    } yield created).transactionally
  }

  def deleteAssociatedAuthorities(course: CourseDb) = {
    for {
      deletedCourseManager <- deleteCourseManagerQuery(course)
      isCourseManager <- isCourseManager(course.lecturer)
      deletedRightsManager <- if (isCourseManager) DBIO.successful(0) else deleteRightsManagerQuery(course.lecturer)
    } yield deletedCourseManager + deletedRightsManager
  }

  def updateAssociatedAuthorities(oldCourse: CourseDb, newCourse: CourseDb) = {
    (deleteAssociatedAuthorities(oldCourse) zip createAssociatedAuthorities(newCourse)).transactionally
  }

  def authoritiesFor(systemId: String): Future[Seq[Authority]] = {
    db.run(filterBy(List(AuthoritySystemIdFilter(systemId))).result.map(_.map(_.toUniqueEntity)))
  }

  override protected def shouldUpdate(existing: AuthorityDb, toUpdate: AuthorityDb): Boolean = {
    (existing.invalidated != toUpdate.invalidated ||
      existing.lastModified != toUpdate.lastModified) &&
      (existing.user == toUpdate.user && existing.course == toUpdate.course && existing.role == toUpdate.role)
  }

  override protected def existsQuery(entity: AuthorityDb): Query[AuthorityTable, AuthorityDb, Seq] = {
    val approximately = List(AuthorityUserFilter(entity.user.toString), AuthorityRoleFilter(entity.role.toString))
    val sufficient = entity.course.fold(approximately)(c => approximately :+ AuthorityCourseFilter(c.toString))
    filterBy(sufficient)
  }

  override protected def toAtomic(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Traversable[AuthorityLike]] = {
    val joinedQuery = for { // TODO wait for https://github.com/slick/slick/issues/179
      ((a, c), l) <- query
        .joinLeft(TableQuery[CourseTable]).on(_.course === _.id)
        .joinLeft(TableQuery[UserTable]).on(_._2.map(_.lecturer) === _.id)
      u <- a.userFk
      r <- a.roleFk
    } yield (a, u, r, c, l)

    val action = joinedQuery.result.map(_.groupBy(_._1.id).map {
      case (id, dependencies) =>
        val (_, user, role, maybeCourse, maybeLecturer) = dependencies.find(_._1.id == id).get
        val maybeCourseAtom = maybeCourse zip maybeLecturer map {
          case (c, l) => CourseAtom(c.label, c.description, c.abbreviation, l.toUniqueEntity, c.semesterIndex, c.id)
        }
        AuthorityAtom(user.toUniqueEntity, role.toUniqueEntity, maybeCourseAtom.headOption, id)
    })

    db.run(action)
  }

  override protected def toUniqueEntity(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Traversable[AuthorityLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class AuthorityDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val roleDao: RoleDao, val executionContext: ExecutionContext) extends AuthorityDao