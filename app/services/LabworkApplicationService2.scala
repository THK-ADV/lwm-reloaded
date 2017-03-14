package services

import java.util.UUID

import models._
import store.{LabworkApplicationFriendTable, LabworkApplicationTable, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

case class ApplicantFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.applicant === UUID.fromString(value)
}

trait LabworkApplicationService2 extends AbstractDao[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override protected def tableQuery: TableQuery[LabworkApplicationTable] = TableQuery[LabworkApplicationTable]

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

    fullJoin.result.statements.foreach(println)

    db.run(fullJoin.result.map(_.groupBy(_._1).map {
      case (labworkApplication, foreigners) => build(labworkApplication, foreigners)
    }.toSeq))
  }

  final def createWithFriends(labworkApplication: LabworkApplicationDb): Future[LabworkApplicationDb] = {
    for {
      lapp <- create(labworkApplication)
      friends = labworkApplication.friends.map(f => LabworkApplicationFriend(lapp.id, f, None, UUID.randomUUID))
      _ <- LabworkApplicationFriendService.createMany(friends)
    } yield labworkApplication
  }
}

trait LabworkApplicationFriendService extends AbstractDao[LabworkApplicationFriendTable, LabworkApplicationFriend, LabworkApplicationFriend] {
  override protected def tableQuery: TableQuery[LabworkApplicationFriendTable] = TableQuery[LabworkApplicationFriendTable]

  override protected def toAtomic(query: Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq]): Future[Seq[LabworkApplicationFriend]] = ???

  override protected def toUniqueEntity(query: Query[LabworkApplicationFriendTable, LabworkApplicationFriend, Seq]): Future[Seq[LabworkApplicationFriend]] = ???
}

object LabworkApplicationService2 extends LabworkApplicationService2
object LabworkApplicationFriendService extends LabworkApplicationFriendService