package dao

import java.util.UUID

import dao.helper.TableFilterable
import database.{LabworkDb, LabworkTable}
import javax.inject.Inject
import models._
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.{ExecutionContext, Future}


object LabworkDao extends TableFilterable[LabworkTable] {
  def publishedFilter(published: Boolean): TableFilterPredicate = _.published === published

  def subscribableFilter(subscribable: Boolean): TableFilterPredicate = _.subscribable === subscribable

  def labelFilter(label: String): TableFilterPredicate = TableFilterable.labelFilterLike(label)

  def courseFilter(course: UUID): TableFilterPredicate = _.course === course

  def semesterFilter(semester: UUID): TableFilterPredicate = _.semester === semester

  def degreeFilter(degree: UUID): TableFilterPredicate = _.degree === degree

  def idFilter(id: UUID): TableFilterPredicate = TableFilterable.idFilter(id)
}

trait LabworkDao extends AbstractDao[LabworkTable, LabworkDb, LabworkLike] {

  import LabworkDao.{courseFilter, degreeFilter, semesterFilter}

  override val tableQuery: TableQuery[LabworkTable] = TableQuery[LabworkTable]

  override protected def shouldUpdate(existing: LabworkDb, toUpdate: LabworkDb): Boolean = {
    (existing.label != toUpdate.label ||
      existing.description != toUpdate.description ||
      existing.subscribable != toUpdate.subscribable ||
      existing.published != toUpdate.published) &&
      (existing.semester == toUpdate.semester && existing.course == toUpdate.course && existing.degree == toUpdate.degree)
  }

  override protected def existsQuery(entity: LabworkDb): Query[LabworkTable, LabworkDb, Seq] = {
    filterBy(List(semesterFilter(entity.semester), courseFilter(entity.course), degreeFilter(entity.degree)))
  }

  override protected def toAtomic(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[LabworkLike]] = {
    val joinedQuery = for {
      q <- query
      c <- q.courseFk
      s <- q.semesterFk
      d <- q.degreeFk
      l <- c.lecturerFk
    } yield (q, c, s, d, l)

    db.run(joinedQuery.result.map(_.map {
      case (l, c, s, d, u) =>
        val courseAtom = CourseAtom(c.label, c.description, c.abbreviation, u.toUniqueEntity, c.semesterIndex, c.id)
        LabworkAtom(l.label, l.description, s.toUniqueEntity, courseAtom, d.toUniqueEntity, l.subscribable, l.published, l.id)
    }))
  }

  override protected def toUniqueEntity(query: Query[LabworkTable, LabworkDb, Seq]): Future[Seq[LabworkLike]] = {
    db.run(query.result.map(_.map(_.toUniqueEntity)))
  }
}

final class LabworkDaoImpl @Inject()(val db: Database, val executionContext: ExecutionContext) extends LabworkDao