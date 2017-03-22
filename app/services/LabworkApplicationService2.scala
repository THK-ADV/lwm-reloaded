package services

import java.util.UUID

import models._
import org.joda.time.DateTime
import store.{LabworkApplicationFriendTable, LabworkApplicationTable, PostgresDatabase, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

case class ApplicantFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.applicant === UUID.fromString(value)
}

trait LabworkApplicationService2 extends AbstractDao[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[LabworkApplicationTable] = TableQuery[LabworkApplicationTable]

  protected def labworkApplicationFriendService: LabworkApplicationFriendService

  override protected def setInvalidated(entity: LabworkApplicationDb): LabworkApplicationDb = {
    LabworkApplicationDb(entity.labwork, entity.applicant, entity.friends, entity.timestamp, Some(DateTime.now), entity.id)
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

      PostgresLabworkApplicationAtom(labworkAtom, applicant.toUser, friends.toSet, lapp.timestamp, lapp.id)
  }

  override protected def toUniqueEntity(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplication]] = joinFriends(query) {
    case (lapp, foreigners) => PostgresLabworkApplication(lapp.labwork, lapp.applicant, foreigners.map(_._2.id).toSet, lapp.timestamp, lapp.id)
  }

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

  final def createWithFriends(labworkApplication: LabworkApplicationDb): Future[LabworkApplicationDb] = {
    for {
      lapp <- create(labworkApplication)
      friends = labworkApplication.friends.map(f => LabworkApplicationFriend(lapp.id, f, None, UUID.randomUUID)).toList
      _ <- labworkApplicationFriendService.createMany(friends)
    } yield labworkApplication
  }

  final def createManyWithFriends(labworkApplications: List[LabworkApplicationDb]): Future[Map[PostgresLabworkApplication, Seq[LabworkApplicationFriend]]] = {
    for {
      lapps <- createMany(labworkApplications)
      friends = labworkApplications.flatMap(l => l.friends.map(f => LabworkApplicationFriend(l.id, f, None, UUID.randomUUID)))
      lafs <- labworkApplicationFriendService.createMany(friends)
    } yield lafs.groupBy(_.labworkApplication).map {
      case (lappId, lappFriends) => (lapps.find(_.id == lappId).map(_.toLabworkApplication).get, lappFriends)
    }
  }
}

trait LabworkApplicationFriendService extends AbstractDao[LabworkApplicationFriendTable, LabworkApplicationFriend, LabworkApplicationFriend] { self: PostgresDatabase =>
  override val tableQuery: TableQuery[LabworkApplicationFriendTable] = TableQuery[LabworkApplicationFriendTable]

  override protected def setInvalidated(entity: LabworkApplicationFriend): LabworkApplicationFriend = {
    LabworkApplicationFriend(entity.labworkApplication, entity.friend, Some(DateTime.now), entity.id)
  }

  override protected def toAtomic(query: Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq]): Future[Seq[LabworkApplicationFriend]] = ???

  override protected def toUniqueEntity(query: Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq]): Future[Seq[LabworkApplicationFriend]] = ???
}

object LabworkApplicationService2 extends LabworkApplicationService2 with PostgresDatabase {
  override protected def labworkApplicationFriendService: LabworkApplicationFriendService = LabworkApplicationFriendService
}

object LabworkApplicationFriendService extends LabworkApplicationFriendService with PostgresDatabase