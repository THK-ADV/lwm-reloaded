package services

import java.util.UUID

import models.LwmDateTime.DateTimeConverter
import models._
import org.joda.time.DateTime
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import slick.lifted.Rep
import store.{LabworkTable, TableFilter}

import scala.concurrent.Future

case class LabworkIdFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.id === UUID.fromString(value)
}

case class LabworkDegreeFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.degree === UUID.fromString(value)
}

case class LabworkSemesterFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.semester === UUID.fromString(value)
}

case class LabworkCourseFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.course === UUID.fromString(value)
}

case class LabworkLabelFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate: (LabworkTable) => Rep[Boolean] = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

trait LabworkService extends AbstractDao[LabworkTable, LabworkDb, Labwork] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[LabworkTable] = TableQuery[LabworkTable]

  override protected def setInvalidated(entity: LabworkDb): LabworkDb = {
    val now = DateTime.now.timestamp

    LabworkDb(
      entity.label,
      entity.description,
      entity.semester,
      entity.course,
      entity.degree,
      entity.subscribable,
      entity.published,
      now,
      Some(now),
      entity.id
    )
  }

  override protected def shouldUpdate(existing: LabworkDb, toUpdate: LabworkDb): Boolean = {
    (existing.label != toUpdate.label ||
      existing.description != toUpdate.description ||
      existing.subscribable != toUpdate.subscribable ||
      existing.published != toUpdate.published) &&
      (existing.semester == toUpdate.semester && existing.course == toUpdate.course && existing.degree == toUpdate.degree)

  }

  override protected def existsQuery(entity: LabworkDb): Query[LabworkTable, LabworkDb, Seq] = {
    filterBy(List(
      LabworkSemesterFilter(entity.semester.toString),
      LabworkCourseFilter(entity.course.toString),
      LabworkDegreeFilter(entity.degree.toString)
    ))
  }

  override protected def toAtomic(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[Labwork]] = {
    val joinedQuery = for {
      q <- query
      c <- q.joinCourse
      s <- q.joinSemester
      d <- q.joinDegree
      l <- c.joinLecturer
    } yield (q, c, s, d, l)

    db.run(joinedQuery.result.map(_.map{
      case (l, c, s, d, u) =>
        val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, u.toUser, c.semesterIndex, c.id)
        PostgresLabworkAtom(l.label, l.description, s.toSemester, courseAtom, d.toDegree, l.subscribable, l.published, l.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[Labwork]] = {
    db.run(query.result.map(_.map(_.toLabwork)))
  }

  protected def courseService: CourseService
  protected def userService: UserService
  protected def degreeService: DegreeService
  protected def semesterService: SemesterService
}

final class LabworkServiceImpl(val db: PostgresDriver.backend.Database,
                                val courseService: CourseService,
                                val userService: UserService,
                                val degreeService: DegreeService,
                                val semesterService: SemesterService) extends LabworkService