package services

import java.util.UUID

import models._
import org.joda.time.DateTime
import store._
import slick.driver.PostgresDriver.api._
import models.LwmDateTime._
import slick.dbio.DBIOAction
import slick.dbio.Effect.Write
import slick.profile.FixedSqlAction

import scala.concurrent.Future

case class LabworkApplicationIdFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.id === UUID.fromString(value)
}

case class LabworkApplicationApplicantFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.applicant === UUID.fromString(value)
}

case class LabworkApplicationLabworkFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

trait LabworkApplicationService2 extends AbstractDao[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[LabworkApplicationTable] = TableQuery[LabworkApplicationTable]

  protected def labworkApplicationFriendService: LabworkApplicationFriendService

  override protected def setInvalidated(entity: LabworkApplicationDb): LabworkApplicationDb = {
    val now = DateTime.now.timestamp

    LabworkApplicationDb(
      entity.labwork,
      entity.applicant,
      entity.friends,
      entity.timestamp,
      now,
      Some(now),
      entity.id
    )
  }

  override protected def shouldUpdate(existing: LabworkApplicationDb, toUpdate: LabworkApplicationDb): Boolean = {
    existing.friends != toUpdate.friends &&
      (existing.applicant == toUpdate.applicant && existing.labwork == toUpdate.labwork)
  }

  override protected def existsQuery(entity: LabworkApplicationDb): Query[LabworkApplicationTable, LabworkApplicationDb, Seq] = {
    filterBy(List(
      LabworkApplicationApplicantFilter(entity.applicant.toString),
      LabworkApplicationLabworkFilter(entity.labwork.toString)
    ))
  }

  override protected def toAtomic(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplication]] = joinFriends(query) {
    case (lapp, foreigners) =>
      val applicant = foreigners.map(_._3).head
      val labwork = foreigners.map(_._4).head
      val semester = foreigners.map(_._5._3).head
      val course = foreigners.map(_._5._1).head
      val lecturer = foreigners.map(_._5._4).head
      val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer.toUser, course.semesterIndex, course.id)
      val degree = foreigners.map(_._5._2).head
      val labworkAtom = PostgresLabworkAtom(labwork.label, labwork.description, semester.toSemester, courseAtom, degree.toDegree, labwork.subscribable, labwork.published, labwork.id)
      val friends = foreigners.map(_._2.toUser)

      PostgresLabworkApplicationAtom(labworkAtom, applicant.toUser, friends.toSet, lapp.timestamp.dateTime, lapp.id)
  }

  override protected def toUniqueEntity(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplication]] = joinFriends(query) {
    case (lapp, foreigners) => PostgresLabworkApplication(lapp.labwork, lapp.applicant, foreigners.map(_._2.id).toSet, lapp.timestamp.dateTime, lapp.id)
  }

  // TODO maybe we can use dedicated queries instead of massive querying... just like user's buddy request
  private def joinFriends(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq])(build: (LabworkApplicationDb, Seq[(LabworkApplicationDb, DbUser, DbUser, LabworkDb, (CourseDb, DegreeDb, SemesterDb, DbUser))]) => LabworkApplication) = {
    val fullJoin = for {
      q <- query
      lapp <- q.fullJoin
      lab <- lapp._3.fullJoin
      lec <- lab._1.joinLecturer
    } yield (q, lapp._1, lapp._2, lapp._3, (lab._1, lab._2, lab._3, lec))

    db.run(fullJoin.result.map(_.groupBy(_._1).map {
      case (labworkApplication, foreigners) => build(labworkApplication, foreigners)
    }.toSeq))
  }

  override protected def databaseExpander: Option[DatabaseExpander[LabworkApplicationDb]] = Some(new DatabaseExpander[LabworkApplicationDb] {
    override def expandCreationOf(entities: Seq[LabworkApplicationDb]) = {
      val friends = entities.flatMap(l => l.friends.map(f => LabworkApplicationFriend(l.id, f)))

      labworkApplicationFriendService.createManyQuery(friends).map(_ => entities)
    }

    override def expandUpdateOf(entity: LabworkApplicationDb) = {
      for {
        deleted <- expandDeleteOf(entity) if deleted.isDefined
        lappFriends = entity.friends.map(id => LabworkApplicationFriend(entity.id, id)).toList
        u <- labworkApplicationFriendService.createManyQuery(lappFriends)
      } yield Some(entity.copy(entity.labwork, entity.applicant, u.map(_.friend).toSet))
    }

    override def expandDeleteOf(entity: LabworkApplicationDb) = {
      labworkApplicationFriendService.deleteFriendsOf(entity.id).map(_ => Some(entity))
    }
  })

  final def friendsOf(applicant: Rep[UUID], labwork: UUID): Query[UserTable, DbUser, Seq] = {
    for {
      buddy <- tableQuery.filter(lapp => lapp.applicant === applicant && lapp.labwork === labwork)
      friends <- labworkApplicationFriendService.tableQuery.filter(_.labworkApplication === buddy.id).flatMap(_.friendFk)
    } yield friends
  }
}

trait LabworkApplicationFriendService extends AbstractDao[LabworkApplicationFriendTable, LabworkApplicationFriend, LabworkApplicationFriend] { self: PostgresDatabase =>
  override val tableQuery: TableQuery[LabworkApplicationFriendTable] = TableQuery[LabworkApplicationFriendTable]

  override protected def setInvalidated(entity: LabworkApplicationFriend): LabworkApplicationFriend = {
    val now = DateTime.now.timestamp

    LabworkApplicationFriend(
      entity.labworkApplication,
      entity.friend,
      now,
      Some(now),
      entity.id
    )
  }

  final def deleteFriendsOf(application: UUID): DBIOAction[Int, NoStream, Write] = {
    tableQuery.filter(_.labworkApplication === application).delete
  }

  override protected def shouldUpdate(existing: LabworkApplicationFriend, toUpdate: LabworkApplicationFriend): Boolean = ???

  override protected def existsQuery(entity: LabworkApplicationFriend): _root_.slick.driver.PostgresDriver.api.Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq] = ???

  override protected def toAtomic(query: Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq]): Future[Seq[LabworkApplicationFriend]] = toUniqueEntity(query)

  override protected def toUniqueEntity(query: Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq]): Future[Seq[LabworkApplicationFriend]] = {
    db.run(query.result)
  }
}

object LabworkApplicationService2 extends LabworkApplicationService2 with PostgresDatabase {
  override protected def labworkApplicationFriendService: LabworkApplicationFriendService = LabworkApplicationFriendService
}

object LabworkApplicationFriendService extends LabworkApplicationFriendService with PostgresDatabase