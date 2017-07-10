package services

import models.LwmDateTime.DateTimeConverter
import models.{Course, CourseDb, PostgresCourseAtom}
import org.joda.time.DateTime
import slick.driver
import slick.driver.PostgresDriver
import slick.driver.PostgresDriver.api._
import store.{CourseTable, TableFilter}

import scala.concurrent.Future

case class CourseLabelFilter(value: String) extends TableFilter[CourseTable] {
  override def predicate = _.label.toLowerCase like s"%${value.toLowerCase}%"
}

case class CourseSemesterIndexFilter(value: String) extends TableFilter[CourseTable] {
  override def predicate = _.semesterIndex === value.toInt
}

case class CourseAbbreviationFilter(value: String) extends TableFilter[CourseTable] {
  override def predicate: (CourseTable) => Rep[Boolean] = _.abbreviation.toLowerCase === value.toLowerCase
}

trait CourseService extends AbstractDao[CourseTable, CourseDb, Course] {

  import scala.concurrent.ExecutionContext.Implicits.global

  override val tableQuery: TableQuery[CourseTable] = TableQuery[CourseTable]

  protected def authorityService: AuthorityService

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
//
//  override protected def databaseExpander: Option[DatabaseExpander[CourseDb]] = Some(new DatabaseExpander[CourseDb] {
//
//    override def expandUpdateOf(entity: CourseDb): DBIOAction[Option[CourseDb], NoStream, Effect.Write] = {
//      authorityService.updateWithCourse(entity).map(_ => Some(entity))
//    }
//
//    override def expandDeleteOf(entity: CourseDb): DBIOAction[Option[CourseDb], NoStream, Effect.Write] = {
//      authorityService.deleteWithCourse(entity).map(_ => Some(entity))
//    }
//
//    override def expandCreationOf[E <: Effect](entities: Seq[CourseDb]): DBIOAction[Seq[CourseDb], NoStream, Effect.Write with E] = {
//      DBIO.sequence(entities.map(authorityService.createWithCourse)).map(_ => entities)
//    }
//  })
}

final class CourseServiceImpl(val db: PostgresDriver.backend.Database, val authorityService: AuthorityService) extends CourseService

