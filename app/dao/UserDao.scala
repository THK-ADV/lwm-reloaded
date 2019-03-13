package dao

import java.util.UUID

import dao.helper.DBResult
import database.helper.{EmployeeStatus, LdapUserStatus, LecturerStatus, StudentStatus}
import database.{TableFilter, UserDb, UserTable}
import javax.inject.Inject
import models._
import models.helper._
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Rep

import scala.concurrent.{ExecutionContext, Future}

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

  override val tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  def degreeDao: DegreeDao

  def authorityDao: AuthorityDao

  def labworkApplicationDao: LabworkApplicationDao

  final def userId(systemId: String) = filterValidOnly(_.systemId === systemId).map(_.id).take(1).result.headOption

  final def makeUser(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]) = {
    for {
      status <- DBIO.from(Future.fromTry(LdapUserStatus(status)))
      maybeDegree <- status match {
        case StudentStatus if enrollment.isDefined && registrationId.isDefined =>
          degreeDao.filterBy(List(DegreeAbbreviationFilter(enrollment.get))).result.flatMap { degrees =>
            degrees.headOption
              .map(d => DBIO.successful(Some(d.id)))
              .getOrElse(DBIO.failed(new Throwable(s"degree with label '${enrollment.get}' not found")))
          }
        case EmployeeStatus | LecturerStatus => DBIO.successful(Option.empty[UUID])
        case _ => DBIO.failed(new Throwable(s"user with $status label must have a associated registration-id and degree abbreviation, but was $registrationId and $enrollment"))
      }
      user = UserDb(systemId, lastname, firstname, email, status, registrationId, maybeDegree)
    } yield user
  }

  final def createOrUpdateWithBasicAuthority(user: UserDb): Future[DBResult[UserDb]] = {
    val result = for {
      existing <- userId(user.systemId)
      createOrUpdated <- existing match {
        case Some(_) => updateQuery(user).map(u => DBResult.Updated(u))
        case None => createWithBasicAuthorityQuery(user).map(t => DBResult.Created(t._1))
      }
    } yield createOrUpdated

    db.run(result)
  }

  final def createOrUpdateWithBasicAuthority(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]): Future[DBResult[UserDb]] = {
    val result = for {
      user <- makeUser(systemId, lastname, firstname, email, status, registrationId, enrollment)
      existing <- userId(user.systemId)
      createOrUpdated <- existing match {
        case Some(_) => updateQuery(user).map(u => DBResult.Updated(u))
        case None => createWithBasicAuthorityQuery(user).map(t => DBResult.Created(t._1))
      }
    } yield createOrUpdated

    db.run(result)
  }

  final def createWithBasicAuthorityQuery(user: UserDb) = {
    (for {
      createdUser <- createQuery(user)
      baseAuth <- authorityDao.createBasicAuthorityFor(user)
    } yield (createdUser, baseAuth)).transactionally
  }

  final def createWithBasicAuthorityQuery(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]) = {
    (for {
      user <- makeUser(systemId, lastname, firstname, email, status, registrationId, enrollment)
      createdUser <- createQuery(user)
      baseAuth <- authorityDao.createBasicAuthorityFor(user)
    } yield (createdUser, baseAuth)).transactionally
  }

  final def buddyResult(requesterId: String, requesteeSystemId: String, labwork: String): Future[BuddyResult] = {
    val requesteeSystemIdFilter = UserSystemIdFilter(requesteeSystemId)
    val requesterIdFilter = UserIdFilter(requesterId)

    val buddy = for {
      requestee <- filterBy(List(requesteeSystemIdFilter))
      requester <- filterBy(List(requesterIdFilter))
      sameDegree = requestee.enrollment === requester.enrollment
    } yield (requestee, sameDegree.getOrElse(false))

    val friends = for {
      b <- buddy
      friends <- labworkApplicationDao.friendsOf(b._1.id, UUID.fromString(labwork))
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

  override protected def toUniqueEntity(query: Query[UserTable, UserDb, Seq]): Future[Traversable[User]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def toAtomic(query: Query[UserTable, UserDb, Seq]): Future[Traversable[User]] = {
    db.run(query.joinLeft(degreeDao.tableQuery).on(_.enrollment === _.id).result.map(_.map {
      case (s, Some(d)) => StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d.toUniqueEntity, s.id)
      case (dbUser, None) => dbUser.toUniqueEntity
    }))
  }
}

final class UserDaoImpl @Inject()(
  val db: Database,
  val authorityDao: AuthorityDao,
  val degreeDao: DegreeDao,
  val labworkApplicationDao: LabworkApplicationDao,
  val executionContext: ExecutionContext
) extends UserDao