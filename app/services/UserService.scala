package services

import java.util.UUID

import models._
import store.{DegreeTable, TableFilter, UserTable}

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
  case object Allowed extends BuddyResult // same degree, in tool, paired
  case object Almost extends BuddyResult // same degree, in tool, not paired yet
  case object Denied extends BuddyResult // not same degree, in tool, other

  final def buddyResult(studentId: String, buddySystemId: String, labwork: String): Future[BuddyResult] = {
    val buddySystemIdFilter = UserSystemIdFilter(buddySystemId)
    val requesterIdFilter = UserIdFilter(studentId)

    val query = for {
      buddy <- tableQuery.filter(buddySystemIdFilter.predicate)
      requester <- tableQuery.filter(requesterIdFilter.predicate)
      sameDegree = buddy.enrollment === requester.enrollment
      buddyApps <- buddy.labworkApplication(UUID.fromString(labwork))
      friends <- buddyApps.friends
    } yield (buddy, requester, sameDegree.getOrElse(false), friends)

    query.result.statements.foreach(println)

    db.run(query.result.map { seq =>
      val sameDegree = seq.map(_._3).reduce(_ && _)

      if (sameDegree) {
        val friends = seq.exists(t => t._2.id == t._4.id)

        if (friends) Allowed else Almost
      }
      else
        Denied
    })
  }
}

object UserService extends UserService