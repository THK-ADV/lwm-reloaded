package services

import models.{Course, CourseDb, PostgresCourseAtom}
import store.{CourseTable, PostgresDatabase}

import scala.concurrent.Future
import slick.driver.PostgresDriver.api._

trait CourseService extends AbstractDao[CourseTable, CourseDb, Course] { self: PostgresDatabase =>
  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[CourseTable] = TableQuery[CourseTable]

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