package services

import java.util.UUID

import models._
import org.joda.time.DateTime
import store.{PostgresDatabase, TableFilter, UserTable}

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep

sealed trait BuddyResult
case object Allowed extends BuddyResult
case object Almost extends BuddyResult
case object Denied extends BuddyResult

case class UserStatusFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.status.toLowerCase === value.toLowerCase
}
case class UserSystemIdFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.systemId.toLowerCase === value.toLowerCase
}
case class UserLastnameFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.lastname.toLowerCase like s"%${value.toLowerCase}%"
}
case class UserFirstnameFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.firstname.toLowerCase like s"%${value.toLowerCase}%"
}
case class UserDegreeFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.enrollment.map(_ === UUID.fromString(value)).getOrElse(false)
}
case class UserIdFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.id === UUID.fromString(value)
}

trait UserService extends AbstractDao[UserTable, DbUser, User] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  protected def degreeService: DegreeService
  protected def authorityService: AuthorityService

  override val tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  override protected def setInvalidated(entity: DbUser): DbUser = {
    DbUser(entity.systemId, entity.lastname, entity.firstname, entity.email, entity.status, entity.registrationId, entity.enrollment, Some(DateTime.now), entity.id)
  }

  override protected def toUniqueEntity(query: Query[UserTable, DbUser, Seq]): Future[Seq[User]] = {
    db.run(query.result.map(_.map(_.toUser)))
  }

  override protected def toAtomic(query: Query[UserTable, DbUser, Seq]): Future[Seq[User]] = {
    db.run(query.joinLeft(degreeService.tableQuery).on(_.enrollment === _.id).result.map(_.map {
      case ((s, Some(d))) => PostgresStudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d.toDegree, s.id)
      case ((dbUser, None)) => dbUser.toUser
    }))
  }

  final def createOrUpdate(ldapUser: LdapUser): Future[(User, Option[PostgresAuthorityAtom])] = {
    for {
      degrees <- degreeService.get()
      existing <- get(List(UserSystemIdFilter(ldapUser.systemId)), atomic = false)
      maybeEnrollment = ldapUser.degreeAbbrev.flatMap(abbrev => degrees.find(_.abbreviation.toLowerCase == abbrev.toLowerCase)).map(_.id)
      dbUser = DbUser(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, ldapUser.registrationId, maybeEnrollment, None, existing.headOption.fold(ldapUser.id)(_.id))
      updated <- createOrUpdate(dbUser)
      maybeAuth <- updated.fold[Future[Option[PostgresAuthorityAtom]]](Future.successful(None))(user => authorityService.createWith(user).map(Some(_)))
    } yield (dbUser.toUser, maybeAuth)
  }

  final def buddyResult(studentId: String, buddySystemId: String, labwork: String): Future[BuddyResult] = {
    val buddySystemIdFilter = UserSystemIdFilter(buddySystemId)
    val requesterIdFilter = UserIdFilter(studentId)

    val buddy = for {
      buddy <- tableQuery.filter(buddySystemIdFilter.predicate)
      requester <- tableQuery.filter(requesterIdFilter.predicate)
      sameDegree = buddy.enrollment === requester.enrollment
    } yield (buddy, sameDegree.getOrElse(false))

    val friends = for {
      b <- buddy
      buddyApp <- b._1.labworkApplication(UUID.fromString(labwork))
      friends <- buddyApp.friends
    } yield friends

    db.run(for {
      b <- buddy.result
      f <- friends.result
    } yield {
      val sameDegree = b.map(_._2).reduce(_ && _)
      val friends = f.exists(_.id == UUID.fromString(studentId))

      if (sameDegree)
        if (friends) Allowed else Almost
      else
        Denied
    })
  }
}

object UserService extends UserService with PostgresDatabase {
  override protected def degreeService: DegreeService = DegreeService

  override protected def authorityService: AuthorityService = AuthorityService
}