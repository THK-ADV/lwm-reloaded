package services

import java.sql.Timestamp

import models.{Course, CourseDb, PostgresCourseAtom}
import org.joda.time.DateTime
import store.{CourseTable, PostgresDatabase, TableFilter}
import models.LwmDateTime.DateTimeConverter
import scala.concurrent.Future
import slick.driver.PostgresDriver.api._

case class CourseLabelFilter(value: String) extends TableFilter[CourseTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class CourseSemesterIndexFilter(value: String) extends TableFilter[CourseTable] {
  override def predicate = _.semesterIndex === value.toInt
}

trait CourseService extends AbstractDao[CourseTable, CourseDb, Course] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[CourseTable] = TableQuery[CourseTable]

  override protected def existsQuery(entity: CourseDb): Query[CourseTable, CourseDb, Seq] = {
    filterBy(List(CourseLabelFilter(entity.label), CourseSemesterIndexFilter(entity.semesterIndex.toString)))
  }

  override protected def shouldUpdate(existing: CourseDb, toUpdate: CourseDb): Boolean = {
    (existing.description != toUpdate.description ||
      existing.abbreviation != toUpdate.abbreviation ||
      existing.lecturer != toUpdate.lecturer) &&
      (existing.semesterIndex == toUpdate.semesterIndex && existing.label == toUpdate.label)
  }

  override protected def setInvalidated(entity: CourseDb): CourseDb = {
    val now = DateTime.now.timestamp

    CourseDb(
      entity.label,
      entity.description,
      entity.abbreviation,
      entity.lecturer,
      entity.semesterIndex,
      now,
      Some(now),
      entity.id
    )
  }

  override protected def toAtomic(query: Query[CourseTable, CourseDb, Seq]): Future[Seq[Course]] = {
    val joinedQuery = for {
      q <- query
      l <- q.joinLecturer
    } yield (q, l)

    db.run(joinedQuery.result.map(_.map {
      case (c, l) => PostgresCourseAtom(c.label, c.description, c.abbreviation, l.toUser, c.semesterIndex, c.id)
    }.toSeq))
  }

  override protected def toUniqueEntity(query: Query[CourseTable, CourseDb, Seq]): Future[Seq[Course]] = {
    db.run(query.result.map(_.map(_.toCourse)))
  }
}

object CourseService extends CourseService with PostgresDatabase