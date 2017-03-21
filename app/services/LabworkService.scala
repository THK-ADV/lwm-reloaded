package services

import java.util.UUID

import models._
import org.joda.time.DateTime
import store.{CourseTable, LabworkTable, PostgresDatabase, TableFilter}
import slick.driver.PostgresDriver.api._

import scala.concurrent.Future

case class LabworkIdFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.id === UUID.fromString(value)
}

case class LabworkDegreeFilter(value: String) extends TableFilter[LabworkTable] {
  override def predicate = _.degree === UUID.fromString(value)
}

trait LabworkService extends AbstractDao[LabworkTable, LabworkDb, Labwork] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[LabworkTable] = TableQuery[LabworkTable]

  override protected def setInvalidated(entity: LabworkDb): LabworkDb = LabworkDb(
    entity.label,
    entity.description,
    entity.semester,
    entity.course,
    entity.degree,
    entity.subscribable,
    entity.published,
    Some(DateTime.now),
    entity.id
  )

  override protected def toAtomic(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[Labwork]] = {
    val joinedQuery = for {
      q <- query
      c <- q.joinCourse
      s <- q.joinSemester
      d <- q.joinDegree
      l <- c.joinLecturer
    } yield (q, c, s, d, l)

    joinedQuery.result.statements.foreach(println)

    db.run(joinedQuery.result.map(_.map{
      case (l, c, s, d, u) =>
        val courseAtom = PostgresCourseAtom(c.label, c.description, c.abbreviation, u.toUser, c.semesterIndex, c.id)
        PostgresLabworkAtom(l.label, l.description, s.toSemester, courseAtom, d.toDegree, l.subscribable, l.published, l.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[Labwork]] = {
    db.run(query.result.map(_.map(_.toLabwork)))
  }
}

object LabworkService extends LabworkService with PostgresDatabase