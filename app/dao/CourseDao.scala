package dao

import dao.helper.TableFilterable
import database.{CourseDb, CourseTable}
import javax.inject.Inject
import models.{CourseAtom, CourseLike}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object CourseDao extends TableFilterable[CourseTable] {
  def labelFilter(label: String): TableFilterPredicate = TableFilterable.labelFilterEquals(label)

  def abbreviationFilter(abbreviation: String): TableFilterPredicate = TableFilterable.abbreviationFilter(abbreviation)

  def semesterIndexFilter(index: Int): TableFilterPredicate = _.semesterIndex === index
}

trait CourseDao extends AbstractDao[CourseTable, CourseDb, CourseLike] {

  import CourseDao._

  override val tableQuery: TableQuery[CourseTable] = TableQuery[CourseTable]

  override protected def existsQuery(entity: CourseDb): Query[CourseTable, CourseDb, Seq] = {
    filterBy(List(labelFilter(entity.label), semesterIndexFilter(entity.semesterIndex)))
  }

  override protected def shouldUpdate(existing: CourseDb, toUpdate: CourseDb): Boolean = {
    (existing.description != toUpdate.description ||
      existing.abbreviation != toUpdate.abbreviation ||
      existing.lecturer != toUpdate.lecturer) &&
      (existing.semesterIndex == toUpdate.semesterIndex && existing.label == toUpdate.label)
  }

  override protected def toAtomic(query: Query[CourseTable, CourseDb, Seq]): Future[Seq[CourseLike]] = {
    val joinedQuery = for {
      q <- query
      l <- q.lecturerFk
    } yield (q, l)

    db.run(joinedQuery.result.map(_.map {
      case (c, l) => CourseAtom(c.label, c.description, c.abbreviation, l.toUniqueEntity, c.semesterIndex, c.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[CourseTable, CourseDb, Seq]): Future[Seq[CourseLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class CourseDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends CourseDao

