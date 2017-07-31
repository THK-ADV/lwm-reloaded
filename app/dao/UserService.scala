package dao

import java.util.UUID

import models.LwmDateTime.DateTimeConverter
import models._
import org.joda.time.DateTime
import services._
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep
import store.{TableFilter, UserTable}

import scala.concurrent.Future

sealed trait BuddyResult

case object Allowed extends BuddyResult

case object Almost extends BuddyResult

case object Denied extends BuddyResult

case object NotExisting extends BuddyResult

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

trait UserService extends AbstractDao[UserTable, DbUser, User] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  final def createOrUpdate(ldapUser: LdapUser): Future[(User, Option[PostgresAuthorityAtom])] = {
    for {
      degrees <- degreeService.get()
      existing <- get(List(UserSystemIdFilter(ldapUser.systemId)), atomic = false)
      maybeEnrollment = ldapUser.degreeAbbrev.flatMap(abbrev => degrees.find(_.abbreviation.toLowerCase == abbrev.toLowerCase)).map(_.id)
      dbUser = DbUser(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, ldapUser.registrationId, maybeEnrollment, DateTime.now.timestamp, None, existing.headOption.fold(ldapUser.id)(_.id))
      updated <- createOrUpdate(dbUser)
      maybeAuth <- updated.fold[Future[Option[PostgresAuthorityAtom]]](Future.successful(None))(user => authorityService.createWith(user).map(Some(_)))
    } yield (dbUser.toLwmModel, maybeAuth)
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
      friends <- labworkApplicationService.friendsOf(b._1.id, UUID.fromString(labwork))
    } yield friends

    db.run(for {
      b <- buddy.result
      f <- friends.result
    } yield {
      val sameDegree = b.map(_._2).reduceOption(_ && _)
      val friends = f.exists(_.id == UUID.fromString(studentId))

      sameDegree.fold[BuddyResult](NotExisting) { sameDegree =>
        if (sameDegree)
          if (friends) Allowed else Almost
        else
          Denied
      }
    })
  }

  protected def degreeService: DegreeService

  protected def authorityService: AuthorityService

  protected def labworkApplicationService: LabworkApplicationService2

  override protected def shouldUpdate(existing: DbUser, toUpdate: DbUser): Boolean = {
    (existing.enrollment != toUpdate.enrollment ||
      existing.lastname != toUpdate.lastname ||
      existing.firstname != toUpdate.firstname ||
      existing.email != toUpdate.email) &&
      existing.systemId == toUpdate.systemId
  }

  override protected def existsQuery(entity: DbUser): Query[UserTable, DbUser, Seq] = {
    filterBy(List(UserSystemIdFilter(entity.systemId)))
  }

  override protected def toUniqueEntity(query: Query[UserTable, DbUser, Seq]): Future[Seq[User]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }

  override protected def toAtomic(query: Query[UserTable, DbUser, Seq]): Future[Seq[User]] = {
    db.run(query.joinLeft(degreeService.tableQuery).on(_.enrollment === _.id).result.map(_.map {
      case ((s, Some(d))) => PostgresStudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d.toLwmModel, s.id)
      case ((dbUser, None)) => dbUser.toLwmModel
    }))
  }
}

final class UserServiceImpl(val db: PostgresDriver.backend.Database,
                            val authorityService: AuthorityService,
                            val degreeService: DegreeService,
                            val labworkApplicationService: LabworkApplicationService2
                           ) extends UserService