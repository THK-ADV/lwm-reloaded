package dao

import dao.helper.TableFilter
import database.{CourseDb, CourseTable}
import javax.inject.Inject
import models.{CourseAtom, CourseLike}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}

object CourseDao extends TableFilter[CourseTable] {
  def semesterIndexFilter(index: Int): TableFilterPredicate = _.semesterIndex === index
}

trait CourseDao extends AbstractDao[CourseTable, CourseDb, CourseLike] {

  import dao.helper.TableFilter.userFilter

  override val tableQuery: TableQuery[CourseTable] = TableQuery[CourseTable]

  override protected def existsQuery(entity: CourseDb): Query[CourseTable, CourseDb, Seq] = {
    filterBy(List(userFilter(entity.lecturer)))
  }

  override protected def shouldUpdate(existing: CourseDb, toUpdate: CourseDb): Boolean = {
    existing.lecturer == toUpdate.lecturer
  }

  override protected def toAtomic(query: Query[CourseTable, CourseDb, Seq]): Future[Seq[CourseLike]] = {
    val joinedQuery = for {
      q <- query
      l <- q.userFk
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

