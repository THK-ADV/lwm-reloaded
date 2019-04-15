package dao

import java.util.UUID

import dao.helper.DatabaseExpander
import database._
import javax.inject.Inject
import models._
import slick.jdbc
import slick.jdbc.PostgresProfile
import slick.jdbc.PostgresProfile.api._
import utils.date.DateTimeOps._

import scala.concurrent.{ExecutionContext, Future}

trait LabworkApplicationDao extends AbstractDao[LabworkApplicationTable, LabworkApplicationDb, LabworkApplicationLike] {

  import dao.helper.TableFilter.{labworkFilter, userFilter}

  type LabworkApplicationDependencies = (LabworkApplicationDb, Seq[((LabworkApplicationDb, LabworkDb, UserDb, (CourseDb, DegreeDb, SemesterDb, UserDb)), Option[UserDb])])
  override val tableQuery: TableQuery[LabworkApplicationTable] = TableQuery[LabworkApplicationTable]
  val lappFriendQuery: TableQuery[LabworkApplicationFriendTable] = TableQuery[LabworkApplicationFriendTable]

  final def friendsOf(applicant: Rep[UUID], labwork: UUID): Query[UserTable, UserDb, Seq] = {
    for {
      app <- filterValidOnly(l => l.user === applicant && l.labwork === labwork)
      friends <- lappFriendQuery if friends.labworkApplication === app.id
      friend <- friends.userFk if friend.isValid && friend != applicant
    } yield friend
  }

  override protected def shouldUpdate(existing: LabworkApplicationDb, toUpdate: LabworkApplicationDb): Boolean = {
    existing.friends != toUpdate.friends &&
      (existing.applicant == toUpdate.applicant && existing.labwork == toUpdate.labwork)
  }

  override protected def existsQuery(entity: LabworkApplicationDb): Query[LabworkApplicationTable, LabworkApplicationDb, Seq] = {
    filterBy(List(userFilter(entity.applicant), labworkFilter(entity.labwork)))
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
    (build: LabworkApplicationDependencies => LabworkApplicationLike) = {
    val mandatory = for {
      q <- query
      l <- q.labworkFk
      a <- q.userFk
      c <- l.courseFk
      d <- l.degreeFk
      s <- l.semesterFk
      lec <- c.userFk
    } yield (q, l, a, (c, d, s, lec))

    db.run(mandatory.
      joinLeft(lappFriendQuery).on(_._1.id === _.labworkApplication).
      joinLeft(TableQuery[UserTable]).on(_._2.map(_.user) === _.id).map {
      case (((lapp, lab, a, (c, d, s, lec)), _), friend) => ((lapp, lab, a, (c, d, s, lec)), friend)
    }.result.map(_.groupBy(_._1._1).map {
      case (labworkApplication, dependencies) => build(labworkApplication, dependencies)
    }.toSeq))
  }

  override protected val databaseExpander: Option[DatabaseExpander[LabworkApplicationDb]] = Some(new DatabaseExpander[LabworkApplicationDb] {
    override def expandCreationOf[X <: Effect](entities: LabworkApplicationDb*): jdbc.PostgresProfile.api.DBIOAction[Seq[LabworkApplicationDb], jdbc.PostgresProfile.api.NoStream, Effect.Write with Any] = {
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
        c <- expandCreationOf(d)
      } yield c.head
    }
  })

  override protected val schemas: List[PostgresProfile.DDL] = List(
    tableQuery.schema,
    lappFriendQuery.schema
  )

  override def createQuery(entity: LabworkApplicationDb) = { // TODO this is business logic, which normally belongs to a service class
    if (entity.friends.contains(entity.applicant))
      DBIO.failed(new Throwable(s"user with id $entity.applicant can't be an applicant and friend for this own application"))
    else
      super.createQuery(entity)
  }
}

final class LabworkApplicationDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends LabworkApplicationDao