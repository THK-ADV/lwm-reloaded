package services

import java.util.UUID

import models._
import services.UserService.Allowed
import store.{DegreeTable, LabworkTable, TableFilter, UserTable}

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep

case class UserStatusFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.status.toLowerCase === value.toLowerCase
}
case class UserSystemIdFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: (UserTable) => Rep[Boolean] = _.systemId.toLowerCase like s"%${value.toLowerCase}%"
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

trait UserService extends AbstractDao[UserTable, DbUser, User] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  override protected def toUniqueEntity(query: Query[UserTable, DbUser, Seq]): Future[Seq[User]] = {
    db.run(query.result.map(_.map(_.toUser)))
  }

  override protected def toAtomic(query: Query[UserTable, DbUser, Seq]): Future[Seq[User]] = {
    db.run(query.joinLeft(TableQuery[DegreeTable]).on(_.enrollment === _.id).result.map(_.map {
      case ((s, Some(d))) => PostgresStudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d.toDegree, s.id)
      case ((dbUser, None)) => dbUser.toUser
    }))
  }

  final def createOrUpdate(ldapUser: LdapUser): Future[(User, Option[PostgresAuthorityAtom])] = {
    for {
      degrees <- DegreeService.get()
      existing <- get(List(UserSystemIdFilter(ldapUser.systemId)), atomic = false)
      maybeEnrollment = ldapUser.degreeAbbrev.flatMap(abbrev => degrees.find(_.abbreviation.toLowerCase == abbrev.toLowerCase)).map(_.id)
      dbUser = DbUser(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, ldapUser.registrationId, maybeEnrollment, None, existing.headOption.fold(ldapUser.id)(_.id))
      updated <- createOrUpdate(dbUser)
      maybeAuth <- updated.fold[Future[Option[PostgresAuthorityAtom]]](Future.successful(None))(user => AuthorityService.createWith(user).mapTo[Option[PostgresAuthorityAtom]])
    } yield (dbUser.toUser, maybeAuth)
  }

  sealed trait BuddyResult
  case object Allowed extends BuddyResult
  case object Almost extends BuddyResult
  case object Denied extends BuddyResult

  final def buddyResult(studentId: String, buddySystemId: String, labwork: String): Future[BuddyResult] = {
    val buddySystemIdFilter = UserSystemIdFilter(buddySystemId)
    //val requesterIdFilter = UserIdFilter(studentId) // TODO FOR TESTING PURPOSE
    val requesterSystemIdFilter = UserSystemIdFilter(studentId)

    val buddy = for {
      buddy <- tableQuery.filter(buddySystemIdFilter.predicate)
      requester <- tableQuery.filter(requesterSystemIdFilter.predicate)
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
      val friends = f.exists(_.systemId == studentId)

      if (sameDegree)
        if (friends) Allowed else Almost
      else
        Denied
    })
  }
}

object UserService extends UserService