package dao

import java.sql.Timestamp
import java.util.UUID

import dao.helper.DatabaseExpander
import database._
import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.LwmDateTime._

import scala.concurrent.{ExecutionContext, Future}

case class LabworkApplicationApplicantFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.applicant === UUID.fromString(value)
}

case class LabworkApplicationLabworkFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

case class LabworkApplicationSinceFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.lastModified >= new Timestamp(value.toLong)
}

case class LabworkApplicationUntilFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.lastModified <= new Timestamp(value.toLong)
}

trait LabworkApplicationDao extends AbstractDao[LabworkApplicationTable, LabworkApplicationDb, LabworkApplicationLike] {

  type LabworkApplicationDependencies = (LabworkApplicationDb, Seq[((LabworkApplicationDb, LabworkDb, UserDb, (CourseDb, DegreeDb, SemesterDb, UserDb)), Option[UserDb])])
  override val tableQuery: TableQuery[LabworkApplicationTable] = TableQuery[LabworkApplicationTable]
  protected val lappFriendQuery: TableQuery[LabworkApplicationFriendTable] = TableQuery[LabworkApplicationFriendTable]

  final def friendsOf(applicant: Rep[UUID], labwork: UUID): Query[UserTable, UserDb, Seq] = {
    for {
      buddy <- tableQuery if buddy.applicant === applicant && buddy.labwork === labwork && buddy.isValid
      friends <- lappFriendQuery if friends.labworkApplication === buddy.id && friends.isValid
      friend <- friends.friendFk if friend.isValid
    } yield friend
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

  override protected def toAtomic(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplicationLike]] = joinDependencies(query) {
    case (labworkApplication, dependencies) =>
      val ((lapp, lab, applicant, (course, degree, semester, lecturer)), _) = dependencies.find(_._1._1.id == labworkApplication.id).get
      val friends = dependencies.flatMap(_._2.map(_.toUniqueEntity))
      val labworkAtom = {
        val courseAtom = CourseAtom(course.label, course.description, course.abbreviation, lecturer.toUniqueEntity, course.semesterIndex, course.id)
        LabworkAtom(lab.label, lab.description, semester.toUniqueEntity, courseAtom, degree.toUniqueEntity, lab.subscribable, lab.published, lab.id)
      }

      LabworkApplicationAtom(labworkAtom, applicant.toUniqueEntity, friends.toSet, lapp.lastModified.dateTime, lapp.id)
  }

  override protected def toUniqueEntity(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplicationLike]] = joinDependencies(query) {
    case (lapp, dependencies) =>
      val friends = dependencies.flatMap(_._2.map(_.id))
      LabworkApplication(lapp.labwork, lapp.applicant, friends.toSet, lapp.lastModified.dateTime, lapp.id)
  }

  private def joinDependencies(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq])
    (build: LabworkApplicationDependencies => LabworkApplicationLike): Future[Seq[LabworkApplicationLike]] = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
      a <- q.joinApplicant
      c <- l.courseFk
      d <- l.degreeFk
      s <- l.semesterFk
      lec <- c.lecturerFk
    } yield (q, l, a, (c, d, s, lec))

    db.run(mandatory.
      joinLeft(lappFriendQuery).on(_._1.id === _.labworkApplication).
      joinLeft(TableQuery[UserTable]).on(_._2.map(_.friend) === _.id).map {
      case (((lapp, lab, a, (c, d, s, lec)), _), friend) => ((lapp, lab, a, (c, d, s, lec)), friend)
    }.result.map(_.groupBy(_._1._1).map {
      case (labworkApplication, dependencies) => build(labworkApplication, dependencies)
    }.toSeq))
  }

  override protected def databaseExpander: Option[DatabaseExpander[LabworkApplicationDb]] = Some(new DatabaseExpander[LabworkApplicationDb] {
    override def expandCreationOf[X <: Effect](entities: Seq[LabworkApplicationDb]) = {
      val friends = entities.flatMap(l => l.friends.map(f => LabworkApplicationFriend(l.id, f)))

      for {
        _ <- lappFriendQuery ++= friends
      } yield entities
    }

    override def expandDeleteOf(entity: LabworkApplicationDb) = for {
      _ <- lappFriendQuery.filter(_.labworkApplication === entity.id).delete
    } yield entity

    override def expandUpdateOf(entity: LabworkApplicationDb) = {
      for {
        d <- expandDeleteOf(entity)
        c <- expandCreationOf(Seq(d))
      } yield c.head
    }
  })

  private lazy val schemas = List(
    tableQuery.schema,
    lappFriendQuery.schema
  )

  override def createSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.map(_.create): _*).transactionally)
  }

  override def dropSchema: Future[Unit] = {
    db.run(DBIO.seq(schemas.reverseMap(_.drop): _*).transactionally)
  }
}

final class LabworkApplicationDaoImpl @Inject()(val db: PostgresProfile.backend.Database, val executionContext: ExecutionContext) extends LabworkApplicationDao