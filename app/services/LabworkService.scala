package services

import java.util.UUID

import models._
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
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

trait LabworkService extends AbstractDao[LabworkTable, LabworkDb, Labwork] {
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[LabworkTable] = TableQuery[LabworkTable]

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
        val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, u.toLwmModel, c.semesterIndex, c.id)
        PostgresLabworkAtom(l.label, l.description, s.toLwmModel, courseAtom, d.toLwmModel, l.subscribable, l.published, l.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[Labwork]] = {
    db.run(query.result.map(_.map(_.toLwmModel)))
  }
}

final class LabworkServiceImpl(val db: PostgresDriver.backend.Database) extends LabworkService