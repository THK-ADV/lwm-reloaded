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

import scala.concurrent.Future

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
  override def predicate: AuthorityTable => Rep[Boolean] = _.roleFk.map(_.label).filter(_ === value).exists
}

trait AuthorityDao extends AbstractDao[AuthorityTable, AuthorityDb, AuthorityLike] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[AuthorityTable] = TableQuery[AuthorityTable]

  protected def roleDao: RoleDao

  def studentRole: LWMRole = Role.StudentRole

  def employeeRole: LWMRole = Role.EmployeeRole

  def courseManagerRole: LWMRole = Role.CourseManager

  def rightsManagerRole: LWMRole = Role.RightsManager

  private def hasAuthority(user: UUID, role: UUID): FixedSqlAction[Boolean, NoStream, Effect.Read] = {
    (for (q <- tableQuery if q.user === user && q.role === role && q.isValid) yield q).exists.result // TODO .isValid should never be called on ever query
  }

  def isCourseManager(user: UUID): FixedSqlAction[Boolean, NoStream, Effect.Read] = {
    (for {
      q <- tableQuery if q.user === user && q.course.isDefined && q.isValid // TODO .isValid should never be called on ever query
      r <- q.roleFk if r.label === courseManagerRole.label
    } yield q).exists.result
  }

  def deleteCourseManagerQuery(course: CourseDb): FixedSqlAction[Int, NoStream, Effect.Write] = {
    tableQuery.filter { q =>
      q.user === course.lecturer &&
        q.course.map(_ === course.id).getOrElse(false) &&
        q.roleFk.filter(_.label === courseManagerRole.label).exists &&
        q.isValid // TODO .isValid should never be called on ever query
    }.delete
  }

  def deleteRightsManagerQuery(user: UUID): FixedSqlAction[Int, NoStream, Effect.Write] = {
    tableQuery.filter { q =>
      q.user === user &&
        q.roleFk.filter(_.label === rightsManagerRole.label).exists &&
        q.isValid // TODO .isValid should never be called on ever query
    }.delete
  }

  def createBasicAuthorityFor(user: UserDb): DBIOAction[AuthorityDb, NoStream, Effect.Read with Effect.Read with Effect.Write] = {
    for {
      baseRole <- roleDao.byUserStatusQuery(user.status) if baseRole.isDefined
      baseAuth = AuthorityDb(user.id, baseRole.get.id)
      created <- createQuery(baseAuth)
    } yield created
  }

  def deleteAuthorityIfNotBasic(id: UUID): Future[AuthorityDb] = {
    val query = for {
      q <- tableQuery if q.id === id && q.isValid // TODO .isValid should never be called on ever query
      r <- q.roleFk if r.label =!= studentRole.label && r.label =!= employeeRole.label
    } yield (q, r)

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

  def updateAssociatedAuthorities(oldCourse: CourseDb, newCourse: CourseDb) = { // TODO inspect, test and refactor
    (deleteAssociatedAuthorities(oldCourse) zip createAssociatedAuthorities(newCourse)).transactionally
  }

  override protected def shouldUpdate(existing: AuthorityDb, toUpdate: AuthorityDb): Boolean = {
    (existing.invalidated != toUpdate.invalidated ||
      existing.lastModified != toUpdate.lastModified) &&
      (existing.user == toUpdate.user && existing.course == toUpdate.course && existing.role == toUpdate.role)
  }

  def authoritiesFor(systemId: String): Future[Seq[Authority]] = { // TODO test
    val authorities = for {
      q <- tableQuery if q.isValid // TODO .isValid should never be called on ever query
      u <- q.userFk if u.systemId === systemId
    } yield q

    db.run(authorities.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def existsQuery(entity: AuthorityDb): Query[AuthorityTable, AuthorityDb, Seq] = {
    val approximately = List(AuthorityUserFilter(entity.user.toString), AuthorityRoleFilter(entity.role.toString))
    val sufficient = entity.course.fold(approximately)(c => approximately :+ AuthorityCourseFilter(c.toString))
    filterBy(sufficient)
  }

  override protected def toAtomic(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[AuthorityLike]] = {
    val joinedQuery = for { // TODO wait for https://github.com/slick/slick/issues/179
      ((a, c), l) <- query.joinLeft(TableQuery[CourseTable]).on(_.course === _.id).
        joinLeft(TableQuery[UserTable]).on(_._2.map(_.lecturer) === _.id)
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
    }.toSeq)

    db.run(action)
  }

  override protected def toUniqueEntity(query: Query[AuthorityTable, AuthorityDb, Seq]): Future[Seq[AuthorityLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class AuthorityDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val roleDao: RoleDao) extends AuthorityDao