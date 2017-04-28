package services

import java.util.UUID

import models._
import org.joda.time.DateTime
import store._
import slick.driver.PostgresDriver.api._
import models.LwmDateTime._
import slick.driver.PostgresDriver

import scala.concurrent.Future

case class LabworkApplicationApplicantFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.applicant === UUID.fromString(value)
}

case class LabworkApplicationLabworkFilter(value: String) extends TableFilter[LabworkApplicationTable] {
  override def predicate = _.labwork === UUID.fromString(value)
}

trait LabworkApplicationService2 extends AbstractDao[LabworkApplicationTable, LabworkApplicationDb, LabworkApplication] {

  import scala.concurrent.ExecutionContext.Implicits.global

  type LabworkApplicationDependencies = (LabworkApplicationDb, Seq[((LabworkApplicationDb, LabworkDb, DbUser, (CourseDb, DegreeDb, SemesterDb, DbUser)), Option[DbUser])])
  override val tableQuery: TableQuery[LabworkApplicationTable] = TableQuery[LabworkApplicationTable]
  protected val lappFriendQuery: TableQuery[LabworkApplicationFriendTable] = TableQuery[LabworkApplicationFriendTable]

  final def friendsOf(applicant: Rep[UUID], labwork: UUID): Query[UserTable, DbUser, Seq] = {
    for {
      buddy <- tableQuery.filter(lapp => lapp.applicant === applicant && lapp.labwork === labwork)
      friends <- lappFriendQuery.filter(_.labworkApplication === buddy.id).flatMap(_.friendFk)
    } yield friends
  }

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

  override protected def toAtomic(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplication]] = joinDependencies(query) {
    case (labworkApplication, dependencies) =>
      val ((lapp, lab, applicant, (course, degree, semester, lecturer)), _) = dependencies.find(_._1._1.id == labworkApplication.id).get
      val friends = dependencies.flatMap(_._2.map(_.toUser))
      val labworkAtom = {
        val courseAtom = PostgresCourseAtom(course.label, course.description, course.abbreviation, lecturer.toUser, course.semesterIndex, course.id)
        PostgresLabworkAtom(lab.label, lab.description, semester.toSemester, courseAtom, degree.toDegree, lab.subscribable, lab.published, lab.id)
      }

      PostgresLabworkApplicationAtom(labworkAtom, applicant.toUser, friends.toSet, lapp.timestamp.dateTime, lapp.id)
  }

  override protected def toUniqueEntity(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq]): Future[Seq[LabworkApplication]] = joinDependencies(query) {
    case (lapp, dependencies) =>
      val friends = dependencies.flatMap(_._2.map(_.id))
      PostgresLabworkApplication(lapp.labwork, lapp.applicant, friends.toSet, lapp.timestamp.dateTime, lapp.id)
  }

  private def joinDependencies(query: Query[LabworkApplicationTable, LabworkApplicationDb, Seq])
                              (build: LabworkApplicationDependencies => LabworkApplication)
  : Future[Seq[LabworkApplication]] = {
    val mandatory = for {
      q <- query
      l <- q.joinLabwork
      a <- q.joinApplicant
      (c, d, s) <- l.fullJoin
      lec <- c.joinLecturer
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
    override def expandCreationOf(entities: Seq[LabworkApplicationDb]) = {
      val friends = entities.flatMap(l => l.friends.map(f => LabworkApplicationFriend(l.id, f)))

      (lappFriendQuery ++= friends).map(_ => entities)
    }

    override def expandUpdateOf(entity: LabworkApplicationDb) = {
      for {
        deleted <- expandDeleteOf(entity) if deleted.isDefined
        created <- expandCreationOf(Seq(entity))
      } yield created.headOption
    }

    override def expandDeleteOf(entity: LabworkApplicationDb) = {
      lappFriendQuery.filter(_.labworkApplication === entity.id).delete.map(_ => Some(entity))
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

final class LabworkApplicationServiceImpl(val db: PostgresDriver.backend.Database) extends LabworkApplicationService2