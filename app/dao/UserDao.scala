package dao

import java.util.UUID

import javax.inject.Inject
import models._
import models.helper._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep
import database.{TableFilter, UserDb, UserTable}

import scala.concurrent.Future

case class UserStatusFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: UserTable => Rep[Boolean] = _.status.toLowerCase === value.toLowerCase
}

case class UserSystemIdFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: UserTable => Rep[Boolean] = _.systemId.toLowerCase === value.toLowerCase
}

case class UserLastnameFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: UserTable => Rep[Boolean] = _.lastname.toLowerCase like s"%${value.toLowerCase}%"
}

case class UserFirstnameFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: UserTable => Rep[Boolean] = _.firstname.toLowerCase like s"%${value.toLowerCase}%"
}

case class UserDegreeFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: UserTable => Rep[Boolean] = _.enrollment.map(_ === UUID.fromString(value)).getOrElse(false)
}

case class UserIdFilter(value: String) extends TableFilter[UserTable] {
  override def predicate: UserTable => Rep[Boolean] = _.id === UUID.fromString(value)
}

trait UserDao extends AbstractDao[UserTable, UserDb, User] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  final def userId(systemId: String): Future[Option[UUID]] = db.run {
    (for (q <- tableQuery if q.systemId === systemId) yield q.id).result.headOption
  }

  //  final def createOrUpdate(ldapUser: LdapUser): Future[(User, Option[PostgresAuthorityAtom])] = { // TODO
  //    for {
  //      degrees <- degreeService.get()
  //      existing <- get(List(UserSystemIdFilter(ldapUser.systemId)), atomic = false)
  //      maybeEnrollment = ldapUser.degreeAbbrev.flatMap(abbrev => degrees.find(_.abbreviation.toLowerCase == abbrev.toLowerCase)).map(_.id)
  //      dbUser = DbUser(ldapUser.systemId, ldapUser.lastname, ldapUser.firstname, ldapUser.email, ldapUser.status, ldapUser.registrationId, maybeEnrollment, id = existing.headOption.fold(UUID.randomUUID)(_.id))
  //      updated <- createOrUpdate(dbUser)
  //      maybeAuth <- updated.fold[Future[Option[PostgresAuthorityAtom]]](Future.successful(None))(user => authorityService.createWith(user).map(Some(_)))
  //    } yield (dbUser.toLwmModel, maybeAuth)
  //  }

  final def buddyResult(requesterId: String, requesteeSystemId: String, labwork: String): Future[BuddyResult] = {
    val requesteeSystemIdFilter = UserSystemIdFilter(requesteeSystemId)
    val requesterIdFilter = UserIdFilter(requesterId)

    val buddy = for {
      requestee <- tableQuery.filter(requesteeSystemIdFilter.predicate)
      requester <- tableQuery.filter(requesterIdFilter.predicate)
      sameDegree = requestee.enrollment === requester.enrollment
    } yield (requestee, sameDegree.getOrElse(false))

    val friends = for {
      b <- buddy
      friends <- labworkApplicationService.friendsOf(b._1.id, UUID.fromString(labwork))
    } yield friends

    val action = for {
      b <- buddy.result
      f <- friends.result
    } yield {
      val optRequestee = b.headOption.map(_._1.toUniqueEntity)
      val optSameDegree = b.map(_._2).reduceOption(_ && _)
      val friends = f.exists(_.id == UUID.fromString(requesterId))

      (optRequestee, optSameDegree) match {
        case (Some(requestee), Some(sameDegree)) =>
          if (sameDegree)
            if (friends) Allowed(requestee) else Almost(requestee)
          else
            Denied(requestee)
        case _ => NotExisting(requesteeSystemId)
      }
    }

    db.run(action)
  }

  protected def degreeService: DegreeDao

  protected def authorityService: AuthorityDao

  protected def labworkApplicationService: LabworkApplicationDao

  override protected def shouldUpdate(existing: UserDb, toUpdate: UserDb): Boolean = {
    (existing.enrollment != toUpdate.enrollment ||
      existing.lastname != toUpdate.lastname ||
      existing.firstname != toUpdate.firstname ||
      existing.email != toUpdate.email) &&
      existing.systemId == toUpdate.systemId
  }

  override protected def existsQuery(entity: UserDb): Query[UserTable, UserDb, Seq] = {
    filterBy(List(UserSystemIdFilter(entity.systemId)))
  }

  override protected def toUniqueEntity(query: Query[UserTable, UserDb, Seq]): Future[Seq[User]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def toAtomic(query: Query[UserTable, UserDb, Seq]): Future[Seq[User]] = {
    db.run(query.joinLeft(degreeService.tableQuery).on(_.enrollment === _.id).result.map(_.map {
      case (s, Some(d)) => StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d.toUniqueEntity, s.id)
      case (dbUser, None) => dbUser.toUniqueEntity
    }))
  }
}

final class UserDaoImpl @Inject()(
  val db: PostgresProfile.backend.Database,
  val authorityService: AuthorityDao,
  val degreeService: DegreeDao,
  val labworkApplicationService: LabworkApplicationDao
) extends UserDao