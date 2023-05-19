package dao

import dao.UserDao.systemIdFilter
import dao.helper.TableFilter.{abbreviationFilter, idFilter}
import dao.helper.{DBResult, TableFilter}
import database.helper.LdapUserStatus
import database.helper.LdapUserStatus._
import database.{DegreeDb, LabworkApplicationTable, UserDb, UserTable}
import models._
import models.helper._
import slick.dbio.Effect
import slick.jdbc.PostgresProfile.api._
import slick.sql.SqlAction

import java.util.UUID
import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

object UserDao extends TableFilter[UserTable] {
  def enrollmentFilter(enrollment: UUID): TableFilterPredicate =
    _.enrollment.map(_ === enrollment).getOrElse(false)

  def firstnameFilter(firstname: String): TableFilterPredicate =
    _.firstname.toLowerCase like s"%${firstname.toLowerCase}%"

  def lastnameFilter(lastname: String): TableFilterPredicate =
    _.lastname.toLowerCase like s"%${lastname.toLowerCase}%"

  def systemIdFilter(systemId: String): TableFilterPredicate =
    _.systemId.toLowerCase === systemId.toLowerCase

  def campusIdFilter(campusId: String): TableFilterPredicate =
    _.campusId.toLowerCase === campusId.toLowerCase

  def statusFilter(status: LdapUserStatus): TableFilterPredicate =
    _.status.toLowerCase === status.label.toLowerCase

  def applicationInCourseFilter(courses: Seq[UUID]): TableFilterPredicate =
    u =>
      TableQuery[LabworkApplicationTable]
        .filter(app => app.user === u.id && app.memberOfCourses(courses))
        .exists
}

trait UserDao extends AbstractDao[UserTable, UserDb, User] {

  override val tableQuery: TableQuery[UserTable] = TableQuery[UserTable]

  def degreeDao: DegreeDao

  def authorityDao: AuthorityDao

  def labworkApplicationDao: LabworkApplicationDao

  def getBySystemId(systemId: String, atomic: Boolean): Future[Option[User]]

  def userId(systemId: String): SqlAction[Option[UUID], NoStream, Effect.Read]

  def buddyResult(
      requesterId: UUID,
      requesteeSystemId: String,
      labwork: UUID
  ): Future[BuddyResult]

  def makeUserModel(
      systemId: String,
      campusId: String,
      lastname: String,
      firstname: String,
      email: String,
      status: String,
      registrationId: Option[String],
      enrollment: Option[String]
  ): Future[UserDb]

  def createOrUpdateWithBasicAuthority(user: UserDb): Future[DBResult[UserDb]]

  def createOrUpdateWithBasicAuthority(
      systemId: String,
      campusId: String,
      lastname: String,
      firstname: String,
      email: String,
      status: String,
      registrationId: Option[String],
      enrollment: Option[String]
  ): Future[DBResult[UserDb]]

  def getByCampusIds(campusIds: List[String]): Future[Seq[User]]
}

final class UserDaoImpl @Inject() (
    val db: Database,
    val authorityDao: AuthorityDao,
    val degreeDao: DegreeDao,
    val labworkApplicationDao: LabworkApplicationDao,
    implicit val executionContext: ExecutionContext
) extends UserDao {

  def getByCampusIds(campusIds: List[String]): Future[Seq[User]] =
    toUniqueEntity(filterValidOnly(_.campusId.toLowerCase.inSet(campusIds)))

  def getBySystemId(systemId: String, atomic: Boolean): Future[Option[User]] =
    getSingleWhere(u => u.systemId === systemId, atomic)

  def userId(systemId: String): SqlAction[Option[UUID], NoStream, Effect.Read] =
    filterValidOnly(systemIdFilter(systemId))
      .map(_.id)
      .take(1)
      .result
      .headOption

  def makeUserModel(
      systemId: String,
      campusId: String,
      lastname: String,
      firstname: String,
      email: String,
      status: String,
      registrationId: Option[String],
      enrollment: Option[String]
  ): Future[UserDb] = {
    def createDegree(abbrev: String): Future[Degree] =
      degreeDao.create(DegreeDb("", abbrev)).map(_.toUniqueEntity)

    for {
      status <- Future.fromTry(LdapUserStatus(status))
      maybeDegree <- status match {
        case StudentStatus
            if enrollment.isDefined && registrationId.isDefined =>
          val degreeAbbrev = enrollment.get
          for {
            maybeDegree <- degreeDao.getSingleWhere(
              abbreviationFilter(degreeAbbrev).apply
            )
            degree <- maybeDegree.fold(createDegree(degreeAbbrev))(
              Future.successful
            )
          } yield Some(degree.id)
        case EmployeeStatus | LecturerStatus =>
          Future.successful(Option.empty[UUID])
        case _ =>
          Future.failed(
            new Throwable(
              s"user with $status label must have a associated registration-id and degree abbreviation, but was $registrationId and $enrollment"
            )
          )
      }
      user = UserDb(
        systemId,
        campusId,
        lastname,
        firstname,
        email,
        status,
        registrationId,
        maybeDegree
      )
    } yield user
  }

  def createOrUpdateWithBasicAuthority(
      user: UserDb
  ): Future[DBResult[UserDb]] = {
    val result = for {
      existing <- userId(user.systemId)
      createOrUpdated <- existing match {
        case Some(_) =>
          updateQuery(user).map(u => DBResult.Updated(u))
        case None =>
          createWithBasicAuthorityQuery(user).map(t => DBResult.Created(t._1))
      }
    } yield createOrUpdated

    db.run(result)
  }

  def createOrUpdateWithBasicAuthority(
      systemId: String,
      campusId: String,
      lastname: String,
      firstname: String,
      email: String,
      status: String,
      registrationId: Option[String],
      enrollment: Option[String]
  ): Future[DBResult[UserDb]] = {
    for {
      user <- makeUserModel(
        systemId,
        campusId,
        lastname,
        firstname,
        email,
        status,
        registrationId,
        enrollment
      )
      res <- createOrUpdateWithBasicAuthority(user)
    } yield res
  }

  def createWithBasicAuthorityQuery(user: UserDb) = {
    (for {
      createdUser <- createQuery(user) // 1
      baseAuth <- authorityDao.createBasicAuthorityFor(user)
    } yield (createdUser, baseAuth)).transactionally
  }

  def buddyResult(
      requesterId: UUID,
      requesteeSystemId: String,
      labwork: UUID
  ): Future[BuddyResult] = {
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

  override protected def shouldUpdate(
      existing: UserDb,
      toUpdate: UserDb
  ): Boolean = {
    existing.systemId == toUpdate.systemId
  }

  override protected def existsQuery(
      entity: UserDb
  ): Query[UserTable, UserDb, Seq] = {
    filterBy(List(systemIdFilter(entity.systemId)))
  }

  override protected def toUniqueEntity(
      query: Query[UserTable, UserDb, Seq]
  ): Future[Seq[User]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }

  override protected def toAtomic(
      query: Query[UserTable, UserDb, Seq]
  ): Future[Seq[User]] = {
    db.run(
      query
        .joinLeft(degreeDao.tableQuery)
        .on(_.enrollment === _.id)
        .result
        .map(_.map {
          case (s, Some(d)) =>
            StudentAtom(
              s.systemId,
              s.campusId,
              s.lastname,
              s.firstname,
              s.email,
              s.registrationId.head,
              d.toUniqueEntity,
              s.id
            )
          case (dbUser, None) => dbUser.toUniqueEntity
        })
    )
  }
}
