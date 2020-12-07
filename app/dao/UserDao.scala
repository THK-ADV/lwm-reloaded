package dao

import java.util.UUID

import dao.UserDao.systemIdFilter
import dao.helper.TableFilter.{abbreviationFilter, idFilter}
import dao.helper.{DBResult, TableFilter}
import database.helper.LdapUserStatus
import database.helper.LdapUserStatus._
import database.{UserDb, UserTable}
import javax.inject.Inject
import models._
import models.helper._
import slick.dbio.Effect
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import slick.sql.SqlAction

import scala.concurrent.{ExecutionContext, Future}

object UserDao extends TableFilter[UserTable] {
  def enrollmentFilter(enrollment: UUID): TableFilterPredicate = _.enrollment.map(_ === enrollment).getOrElse(false)

  def firstnameFilter(firstname: String): TableFilterPredicate = _.firstname.toLowerCase like s"%${firstname.toLowerCase}%"

  def lastnameFilter(lastname: String): TableFilterPredicate = _.lastname.toLowerCase like s"%${lastname.toLowerCase}%"

  def systemIdFilter(systemId: String): TableFilterPredicate = _.systemId.toLowerCase === systemId.toLowerCase

  def statusFilter(status: LdapUserStatus): TableFilterPredicate = _.status.toLowerCase === status.label.toLowerCase
}

trait UserDao extends AbstractDao[UserTable, UserDb, User] {

  override val tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  def degreeDao: DegreeDao

  def authorityDao: AuthorityDao

  def labworkApplicationDao: LabworkApplicationDao

  def get(systemId: String, atomic: Boolean): Future[Option[User]]

  def userId(systemId: String): SqlAction[Option[UUID], NoStream, Effect.Read]

  def buddyResult(requesterId: UUID, requesteeSystemId: String, labwork: UUID): Future[BuddyResult]

  def makeUserModel(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]): Future[UserDb]

  def createOrUpdateWithBasicAuthority(user: UserDb): Future[DBResult[UserDb]]

  def createOrUpdateWithBasicAuthority(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]): Future[DBResult[UserDb]]
}

final class UserDaoImpl @Inject()(
  val db: Database,
  val authorityDao: AuthorityDao,
  val degreeDao: DegreeDao,
  val labworkApplicationDao: LabworkApplicationDao,
  implicit val executionContext: ExecutionContext
) extends UserDao {

  def get(systemId: String, atomic: Boolean): Future[Option[User]] =
    getSingleWhere(u => u.systemId === systemId, atomic)

  def userId(systemId: String): SqlAction[Option[UUID], NoStream, Effect.Read] = filterValidOnly(systemIdFilter(systemId)).map(_.id).take(1).result.headOption

  def makeUserModel(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]): Future[UserDb] = {
    println("makeUserModel")
    val action = for {
      status <- DBIO.from(Future.fromTry(LdapUserStatus(status)))
      maybeDegree <- status match {
        case StudentStatus if enrollment.isDefined && registrationId.isDefined =>
          degreeDao.filterBy(List(abbreviationFilter(enrollment.get))).result.flatMap { degrees =>
            degrees.headOption
              .map(d => DBIO.successful(Some(d.id)))
              .getOrElse(DBIO.failed(new Throwable(s"degree with label '${enrollment.get}' not found")))
          }
        case EmployeeStatus | LecturerStatus => DBIO.successful(Option.empty[UUID])
        case _ => DBIO.failed(new Throwable(s"user with $status label must have a associated registration-id and degree abbreviation, but was $registrationId and $enrollment"))
      }
      user = UserDb(systemId, lastname, firstname, email, status, registrationId, maybeDegree)
    } yield user

    db.run(action)
  }

  def createOrUpdateWithBasicAuthority(user: UserDb): Future[DBResult[UserDb]] = {
    println("createOrUpdateWithBasicAuthority", user)
    val result = for {
      existing <- userId(user.systemId)
      _ = println("existing", existing)
      createOrUpdated <- existing match {
        case Some(_) =>
          println("should not happen")
          updateQuery(user).map(u => DBResult.Updated(u))
        case None =>
          println("create...")
          createWithBasicAuthorityQuery(user).map(t => DBResult.Created(t._1))
      }
    } yield createOrUpdated

    db.run(result)
  }

  def createOrUpdateWithBasicAuthority(systemId: String, lastname: String, firstname: String, email: String, status: String, registrationId: Option[String], enrollment: Option[String]): Future[DBResult[UserDb]] = {
    println("createOrUpdateWithBasicAuthority")

    for {
      user <- makeUserModel(systemId, lastname, firstname, email, status, registrationId, enrollment)
      res <- createOrUpdateWithBasicAuthority(user)
    } yield res
  }

  def createWithBasicAuthorityQuery(user: UserDb) = {
    println("createWithBasicAuthorityQuery", user)
    (for {
      createdUser <- createQuery(user) // 1
      baseAuth <- authorityDao.createBasicAuthorityFor(user)
    } yield (createdUser, baseAuth)).transactionally
  }

  def buddyResult(requesterId: UUID, requesteeSystemId: String, labwork: UUID): Future[BuddyResult] = {
    val requesteeSystemIdFilter = systemIdFilter(requesteeSystemId)
    val requesterIdFilter = idFilter(requesterId)

    val buddy = for {
      requestee <- filterBy(List(requesteeSystemIdFilter))
      requester <- filterBy(List(requesterIdFilter))
      sameDegree = requestee.enrollment === requester.enrollment
    } yield (requestee, sameDegree.getOrElse(false))

    val friends = for {
      b <- buddy
      friends <- labworkApplicationDao.friendsOf(b._1.id, labwork)
    } yield friends

    val action = for {
      b <- buddy.result
      f <- friends.result
    } yield {
      val optRequestee = b.headOption.map(_._1.toUniqueEntity)
      val optSameDegree = b.map(_._2).reduceOption(_ && _)
      val friends = f.exists(_.id == requesterId)

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
    existing.systemId == toUpdate.systemId
  }

  override protected def existsQuery(entity: UserDb): Query[UserTable, UserDb, Seq] = {
    filterBy(List(systemIdFilter(entity.systemId)))
  }

  override protected def toUniqueEntity(query: Query[UserTable, UserDb, Seq]): Future[Seq[User]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def toAtomic(query: Query[UserTable, UserDb, Seq]): Future[Seq[User]] = {
    db.run(query.joinLeft(degreeDao.tableQuery).on(_.enrollment === _.id).result.map(_.map {
      case (s, Some(d)) => StudentAtom(s.systemId, s.lastname, s.firstname, s.email, s.registrationId.head, d.toUniqueEntity, s.id)
      case (dbUser, None) => dbUser.toUniqueEntity
    }))
  }
}